%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
-module(mod_auth).
-moduledoc """
User authentication using text files, Dets, or Mnesia database.

This module provides for basic user authentication using textual files, Dets
databases, or Mnesia databases.

### See also

`m:httpd`, `m:mod_alias`
""".

%% The functions that the webbserver call on startup stop
%% and when the server traverse the modules.
-export([do/1, store/2, remove/1]).

%% User entries to the gen-server.
-export([add_user/2, add_user/5, add_user/6, 
	 add_group_member/3, add_group_member/4, add_group_member/5, 
	 list_users/1, list_users/2, list_users/3, 
	 delete_user/2, delete_user/3, delete_user/4,
	 delete_group_member/3, delete_group_member/4, delete_group_member/5, 
	 list_groups/1, list_groups/2, list_groups/3, 
	 delete_group/2, delete_group/3, delete_group/4,
	 get_user/2, get_user/3, get_user/4, 
	 list_group_members/2, list_group_members/3, list_group_members/4,
	 update_password/6, update_password/5]).

-include("httpd.hrl").
-include("mod_auth.hrl").
-include("httpd_internal.hrl").

-define(VMODULE,"AUTH").

-define(NOPASSWORD,"NoPassword").

-type httpd_user() :: #httpd_user{}.
-type httpd_group() :: #httpd_group{}.

%%====================================================================
%% Internal application API
%%====================================================================	     

-doc false.
do(Info) ->
    case proplists:get_value(status,Info#mod.data) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case proplists:get_value(response, Info#mod.data) of
		%% No response has been generated!
		undefined ->
		    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
					  Info#mod.request_uri),
		    %% Is it a secret area?
		    case secretp(Path,Info#mod.config_db) of
			{yes, {Directory, DirectoryData}} ->
			    case allow((Info#mod.init_data)#init_data.peername,
				       Info#mod.socket_type,Info#mod.socket,
				       DirectoryData) of
				allowed ->
				    case deny((Info#mod.init_data)#init_data.peername,
					      Info#mod.socket_type, 
					      Info#mod.socket,
					      DirectoryData) of
					not_denied ->
					    case proplists:get_value(auth_type,
								     DirectoryData) of
						undefined ->
						    {proceed, Info#mod.data};
						none ->
						    {proceed, Info#mod.data};
						AuthType ->
						    do_auth(Info, 
							    Directory, 
							    DirectoryData,
							    AuthType)
					    end;
					{denied, Reason} ->
					    {proceed,
					     [{status, {403,
							Info#mod.request_uri,
							Reason}}|
					      Info#mod.data]}
				    end;
				{not_allowed, Reason} ->
				    {proceed,[{status,{403,
						       Info#mod.request_uri,
						       Reason}} |
					      Info#mod.data]}
			    end;
			no ->
			    {proceed, Info#mod.data}
		    end;
		%% A response has been generated or sent!
		_Response ->
		    {proceed, Info#mod.data}
	    end
    end.


%% mod_auth recognizes the following Configuration Directives:
%% <Directory /path/to/directory>
%%  AuthDBType
%%  AuthName
%%  AuthUserFile
%%  AuthGroupFile
%%  AuthAccessPassword
%%  require
%%  allow
%% </Directory>

%% When a <Directory> directive is found, a new context is set to
%% [{directory, Directory, DirData}|OtherContext]
%% DirData in this case is a key-value list of data belonging to the
%% directory in question.
%%
%% When the </Directory> statement is found, the Context created earlier
%% will be returned as a ConfigList and the context will return to the
%% state it was previously.

-doc false.
store({directory, {Directory, DirData}}, ConfigList) 
  when is_list(Directory) andalso is_list(DirData) ->
    try directory_config_check(Directory, DirData) of
	ok ->
	    store_directory(Directory, DirData, ConfigList) 
    catch
	throw:Error ->
	    {error, Error, {directory, Directory, DirData}}
    end;
store({directory, {Directory, DirData}}, _) ->
    {error, {wrong_type, {directory, {Directory, DirData}}}}.

-doc false.
remove(ConfigDB) ->
    lists:foreach(fun({directory, {_Dir, DirData}}) -> 
			  AuthMod = auth_mod_name(DirData),
			  (catch apply(AuthMod, remove, [DirData]))
		  end,
		  ets:match_object(ConfigDB,{directory,{'_','_'}})),

    Addr = httpd_util:lookup(ConfigDB, bind_address, undefined),
    Port = httpd_util:lookup(ConfigDB, port),
    Profile = httpd_util:lookup(ConfigDB, profile, ?DEFAULT_PROFILE),
    mod_auth_server:stop(Addr, Port, Profile),
    ok.

-doc(#{equiv => add_user/6}).
-spec add_user(UserName, Options) -> true | {error, Reason} when
      UserName :: string(),
      Options :: [{password,Password} | {userData,UserData} | {port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Password :: string(),
      UserData :: term(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      AuthPassword ::string(),
      Reason :: term().
add_user(UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd}->
	    case get_options(Opt, userData) of
		{error, Reason}->
		    {error, Reason};
		{UserData, Password}->
		    User = [#httpd_user{username  = UserName, 
					password  = Password,
					user_data = UserData}],
		    mod_auth_server:add_user(Addr, Port, Dir, User, AuthPwd)
	    end
    end.


-doc(#{equiv => add_user/6}).
-spec add_user(UserName, Password, UserData, Port, Directory) -> true | {error, Reason} when
      UserName :: string(),
      Password :: string(),
      UserData :: term(),
      Port :: inet:port_number(),
      Directory :: string(),
      Reason :: term().
add_user(UserName, Password, UserData, Port, Dir) ->
    add_user(UserName, Password, UserData, undefined, Port, Dir).
-doc """
add_user(UserName, Password, UserData, Address, Port, Dir) -> true | {error,
Reason}

`add_user/2, add_user/5`, and [`add_user/6`](`add_user/6`) each adds a user to
the user database. If the operation is successful, this function returns `true`.
If an error occurs, `{error, Reason}` is returned. When
[`add_user/2`](`add_user/2`) is called, options `Password`, `UserData`, `Port`,
and `Dir` are mandatory.
""".
-spec add_user(UserName, Password, UserData, Address, Port, Directory) -> true | {error, Reason} when
      UserName :: string(),
      Password :: string(),
      UserData :: term(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Reason :: term().
add_user(UserName, Password, UserData, Addr, Port, Dir) ->
    User = [#httpd_user{username  = UserName, 
			password  = Password,
			user_data = UserData}],
    mod_auth_server:add_user(Addr, Port, Dir, User, ?NOPASSWORD).

-doc(#{equiv => get_user/4}).
-spec get_user(UserName, Options) -> {ok, User} | {error, Reason} when
      UserName :: string(),
      Options :: [{port, Port} | {addr, Address} | {dir, Directory} | {authPassword, AuthPassword}],
      User :: httpd_user(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      AuthPassword :: string(),
      Reason :: term().
get_user(UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:get_user(Addr, Port, Dir, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => get_user/4}).
-spec get_user(UserName, Port, Directory) -> {ok, User} | {error, Reason} when
      UserName :: string(),
      User :: httpd_user(),
      Port :: inet:port_number(),
      Directory :: string(),
      Reason :: term().
get_user(UserName, Port, Dir) ->
    get_user(UserName, undefined, Port, Dir).
-doc """
`get_user/2`, `get_user/3`, and `get_user/4` each returns an `t:httpd_user/0`
record containing the userdata for a specific user. If the user cannot be
found, `{error, Reason}` is returned. When `get_user/2` is called, options
`Port` and `Dir` are mandatory.
""".
-spec get_user(UserName, Address, Port, Directory) -> {ok, User} | {error, Reason} when
      UserName :: string(),
      User :: httpd_user(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Reason :: term().
get_user(UserName, Addr, Port, Dir) ->
    mod_auth_server:get_user(Addr, Port, Dir, UserName, ?NOPASSWORD).

-doc(#{equiv => add_group_member/5}).
-spec add_group_member(GroupName, UserName, Options) -> true | {error, Reason} when
      GroupName :: string(),
      UserName :: string(),
      Options :: [{port, Port} | {addr, Address} | {dir, Directory} | {authPassword, AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      AuthPassword :: string(),
      Reason :: term().

add_group_member(GroupName, UserName, Opt)->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd}->
	    mod_auth_server:add_group_member(Addr, Port, Dir, 
					     GroupName, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => add_group_member/5}).
-spec add_group_member(GroupName, UserName, Port, Directory) -> true | {error, Reason} when
      GroupName :: string(),
      UserName :: string(),
      Port :: inet:port_number(),
      Directory :: string(),
      Reason :: term().
add_group_member(GroupName, UserName, Port, Dir) ->
    add_group_member(GroupName, UserName, undefined, Port, Dir).

-doc """
`add_group_member/3`, `add_group_member/4`, and `add_group_member/5` each adds
a user to a group. If the group does not exist, it is created and the user is
added to the group. Upon successful operation, this function returns `true`.
When `add_group_members/3` is called, options `Port` and `Dir` are mandatory.
""".
-spec add_group_member(GroupName, UserName, Address, Port, Directory) -> true | {error, Reason} when
      GroupName :: string(),
      UserName :: string(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Reason :: term().
add_group_member(GroupName, UserName, Addr, Port, Dir) ->
    mod_auth_server:add_group_member(Addr, Port, Dir, 
				     GroupName, UserName, ?NOPASSWORD).

-doc(#{equiv => delete_group_member/5}).
-spec delete_group_member(GroupName, UserName, Options) -> true | {error, Reason} when
      GroupName :: string(),
      UserName :: string(),
      Options :: [{port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      AuthPassword :: string(),
      Reason :: term().
delete_group_member(GroupName, UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:delete_group_member(Addr, Port, Dir, 
						GroupName, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => delete_group_member/5}).
-spec delete_group_member(GroupName, UserName, Port, Directory) -> true | {error, Reason} when
      GroupName :: string(),
      UserName :: string(),
      Port :: inet:port_number(),
      Directory :: string(),
      Reason :: term().

delete_group_member(GroupName, UserName, Port, Dir) ->
    delete_group_member(GroupName, UserName, undefined, Port, Dir).
-doc """
`delete_group_member/3`, `delete_group_member/4`, and `delete_group_member/5`
each deletes a user from a group. If the group or the user does not exist, this
function returns an error, otherwise `true`. When `delete_group_member/3` is
called, the options `Port` and `Dir` are mandatory.
""".
-spec delete_group_member(GroupName, UserName, Address, Port, Directory) -> true | {error, Reason} when
      GroupName :: string(),
      UserName :: string(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Reason :: term().

delete_group_member(GroupName, UserName, Addr, Port, Dir) ->
    mod_auth_server:delete_group_member(Addr, Port, Dir, 
					GroupName, UserName, ?NOPASSWORD).

-doc(#{equiv => list_users/3}).
-spec list_users(Options) -> {ok, Users} | {error, Reason} when
      Options :: [{port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Users :: [httpd_user()],
      AuthPassword :: string(),
      Reason :: atom().

list_users(Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:list_users(Addr, Port, Dir, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => list_users/3}).
-doc(#{since => <<"OTP R14B01">>}).
-spec list_users(Port, Directory) -> {ok, Users} | {error, Reason} when
      Port :: inet:port_number(),
      Directory :: string(),
      Users :: [httpd_user()],
      Reason :: atom().
list_users(Port, Dir) ->
    list_users(undefined, Port, Dir).
-doc """
`list_users/1`, `list_users/2`, and `list_users/3` each returns a list of users
in the user database for a specific `Port/Dir`. When `list_users/1` is called,
options `Port` and `Dir` are mandatory.
""".
-spec list_users(Address, Port, Directory) -> {ok, Users} | {error, Reason} when
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Users :: [httpd_user()],
      Reason :: atom().

list_users(Addr, Port, Dir) ->
    mod_auth_server:list_users(Addr, Port, Dir, ?NOPASSWORD).

-doc(#{equiv => delete_user/4}).
-spec delete_user(UserName, Options) -> true | {error, Reason} when
      UserName :: string(),
      Options :: [{port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      AuthPassword :: string(),
      Reason :: term().
delete_user(UserName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:delete_user(Addr, Port, Dir, UserName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => delete_user/4}).
-spec delete_user(UserName, Port, Directory) -> true | {error, Reason} when
      UserName :: string(),
      Port :: inet:port_number(),
      Directory :: string(),
      Reason :: term().
delete_user(UserName, Port, Dir) ->
    delete_user(UserName, undefined, Port, Dir).
-doc """
`delete_user/2`, `delete_user/3`, and `delete_user/4` each
deletes a user from the user database. If the operation is successful, this
function returns `true`. If an error occurs, `{error, Reason}` is returned. When
`delete_user/2` is called, options `Port` and `Dir` are
mandatory.
""".
-spec delete_user(UserName, Address, Port, Directory) -> true | {error, Reason} when
      UserName :: string(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Reason :: term().
delete_user(UserName, Addr, Port, Dir) ->
    mod_auth_server:delete_user(Addr, Port, Dir, UserName, ?NOPASSWORD).

-doc(#{equiv => delete_group/4}).
-spec delete_group(GroupName, Options) -> true | {error,Reason} when
      GroupName :: string(),
      Options :: [{port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      AuthPassword :: string(),
      Reason :: term().
delete_group(GroupName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:delete_group(Addr, Port, Dir, GroupName, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc (#{equiv => delete_group(GroupName, undefined, Port, Dir)}).
-spec delete_group(GroupName, Port, Directory) -> true | {error, Reason} when
      GroupName :: string(),
      Port :: inet:port_number(),
      Directory :: string(),
      Reason :: term().
delete_group(GroupName, Port, Dir) ->
    delete_group(GroupName, undefined, Port, Dir).
-doc """
`delete_group/2`, `delete_group/3`, and `delete_group/4` each deletes the group
specified and returns `true`. If there is an error, `{error, Reason}` is
returned. When `delete_group/2` is called, option `Port` and `Dir` are
mandatory.
""".
-spec delete_group(GroupName, Address, Port, Directory) -> true | {error, Reason} when
      GroupName :: string(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Reason :: term().
delete_group(GroupName, Addr, Port, Dir) ->
    mod_auth_server:delete_group(Addr, Port, Dir, GroupName, ?NOPASSWORD).

-doc(#{equiv => list_groups/3}).
-spec list_groups(Options) -> {ok, Groups} | {error, Reason} when
      Options :: [{port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Groups :: [httpd_group()],
      AuthPassword :: string(),
      Reason :: term().
list_groups(Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:list_groups(Addr, Port, Dir, AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => list_groups/3}).
-spec list_groups(Port, Directory) -> {ok, Groups} | {error, Reason} when
      Port :: inet:port_number(),
      Directory :: string(),
      Groups :: [httpd_group()],
      Reason :: term().
list_groups(Port, Dir) ->
    list_groups(undefined, Port, Dir).
-doc """
`list_groups/1`, `list_groups/2`, and `list_groups/3` each lists all the groups
available. If there is an error, `{error, Reason}` is returned. When
`list_groups/1` is called, options `Port` and `Dir` are mandatory.
""".
-spec list_groups(Address, Port, Directory) -> {ok, Groups} | {error, Reason} when
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Directory :: string(),
      Groups :: [httpd_group()],
      Reason :: term().
list_groups(Addr, Port, Dir) ->
    mod_auth_server:list_groups(Addr, Port, Dir, ?NOPASSWORD).

-doc(#{equiv => list_group_members/4}).
-spec list_group_members(GroupName, Options) -> {ok, Users} | {error, Reason} when
      GroupName :: string(),
      Options :: [{port,Port} | {addr,Address} | {dir,Directory} | {authPassword,AuthPassword}],
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address()| string() | undefined,
      Directory :: string(),
      Users :: [httpd_user()],
      AuthPassword :: string(),
      Reason :: term().

list_group_members(GroupName, Opt) ->
    case get_options(Opt, mandatory) of
	{Addr, Port, Dir, AuthPwd} ->
	    mod_auth_server:list_group_members(Addr, Port, Dir, GroupName, 
					       AuthPwd);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc(#{equiv => list_group_members/4}).
-spec list_group_members(GroupName, Port, Directory) -> {ok, Users} | {error, Reason} when
      GroupName :: string(),
      Port :: inet:port_number(),
      Directory :: string(),
      Users :: [httpd_user()],
      Reason :: term().
list_group_members(GroupName, Port, Dir) ->
    list_group_members(GroupName, undefined, Port, Dir).
-doc """
`list_group_members/2`, `list_group_members/3`, and `list_group_members/4` each
lists the members of a specified group. If the group does not exist or there is
an error, `{error, Reason}` is returned. When `list_group_members/2` is called,
options `Port` and `Dir` are mandatory.
""".
-spec list_group_members(GroupName, Address, Port, Directory) -> {ok, Users} | {error, Reason} when
      GroupName :: string(),
      Port :: inet:port_number(),
      Address :: inet:ip4_address() | inet:ip6_address()| string() | undefined,
      Directory :: string(),
      Users :: [httpd_user()],
      Reason :: term().
list_group_members(GroupName, Addr, Port, Dir) ->
    mod_auth_server:list_group_members(Addr, Port, Dir, 
				       GroupName, ?NOPASSWORD).

-doc(#{equiv => update_password/6}).
-spec update_password(Port, Dir, OldPassword, NewPassword, NewPassword) -> ok | {error, Reason} when
      Port :: inet:port_number(),
      Dir :: string(),
      OldPassword :: string(),
      NewPassword :: string(),
      Reason :: term().
update_password(Port, Dir, Old, New, New)->
    update_password(undefined, Port, Dir, Old, New, New).

-doc """
`update_password/5` and `update_password/6` each updates `AuthAccessPassword`
for the specified directory. If `NewPassword` is equal to "NoPassword", no
password is required to change authorisation data. If `NewPassword` is equal to
"DummyPassword", no changes can be done without changing the password first.
""".
-spec update_password(Address, Port, Dir, OldPassword, NewPassword, NewPassword) -> ok | {error, Reason} when
      Address :: inet:ip4_address() | inet:ip6_address() | string() | undefined,
      Port :: inet:port_number(),
      Dir :: string(),
      OldPassword :: string(),
      NewPassword :: string(),
      Reason :: term().
update_password(Addr, Port, Dir, Old, New, New) when is_list(New) ->
    mod_auth_server:update_password(Addr, Port, Dir, Old, New);

update_password(_Addr, _Port, _Dir, _Old, New, New) ->
    {error, badtype};
update_password(_Addr, _Port, _Dir, _Old, _New, _New1) ->
    {error, notqeual}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_auth(Info, Directory, DirectoryData, _AuthType) ->
    %% Authenticate (require)
    case require(Info, Directory, DirectoryData) of
	authorized ->
	    {proceed,Info#mod.data};
	{authorized, User} ->
	    {proceed, [{remote_user,User}|Info#mod.data]};
	{authorization_required, Realm} ->
	    ReasonPhrase = httpd_util:reason_phrase(401),
	    Message = httpd_util:message(401,none,Info#mod.config_db),
	    {proceed,
	     [{response,
	       {401,
		["WWW-Authenticate: Basic realm=\"",Realm,
		 "\"\r\n\r\n","<HTML>\n<HEAD>\n<TITLE>",
		 ReasonPhrase,"</TITLE>\n",
		 "</HEAD>\n<BODY>\n<H1>",ReasonPhrase,
		 "</H1>\n",Message,"\n</BODY>\n</HTML>\n"]}}|
	      Info#mod.data]};
	{status, {StatusCode,PhraseArgs,Reason}} ->
	    {proceed, [{status,{StatusCode,PhraseArgs,Reason}}|
		       Info#mod.data]}
    end.

require(Info, Directory, DirectoryData) ->
    ParsedHeader = Info#mod.parsed_header,
    ValidUsers   = proplists:get_value(require_user, DirectoryData),
    ValidGroups  = proplists:get_value(require_group, DirectoryData),
    %% Any user or group restrictions?
    case ValidGroups of
	undefined when ValidUsers =:= undefined ->
	    authorized;
	_ ->
	    case proplists:get_value("authorization", ParsedHeader) of
		undefined ->
		    authorization_required(DirectoryData);
		%% Check credentials!
		"Basic" ++ EncodedString = Credentials ->
		    case (catch base64:decode_to_string(EncodedString)) of
			{'EXIT',{function_clause, _}} ->
			    {status, {401, none, ?NICE("Bad credentials "++
						       Credentials)}};
			DecodedString ->
			   validate_user(Info, Directory, DirectoryData,
					 ValidUsers, ValidGroups, 
					 DecodedString)
		    end;
		%% Bad credentials!
		BadCredentials ->
		    {status, {401, none, ?NICE("Bad credentials "++
					       BadCredentials)}}
	    end
    end.

authorization_required(DirectoryData) ->
    case proplists:get_value(auth_name, DirectoryData) of
	undefined ->
	    {status,{500, none,?NICE("AuthName directive not specified")}};
	Realm ->
	    {authorization_required, Realm}
    end.


validate_user(Info, Directory, DirectoryData, ValidUsers, 
	      ValidGroups, DecodedString) ->
    case a_valid_user(Info, DecodedString, 
		      ValidUsers, ValidGroups, 
		      Directory, DirectoryData) of
	{yes, User} ->
	    {authorized, User};
	{no, _Reason} ->
	    authorization_required(DirectoryData);
	{status, {StatusCode,PhraseArgs,Reason}} ->
	    {status,{StatusCode,PhraseArgs,Reason}}
    end.

a_valid_user(Info,DecodedString,ValidUsers,ValidGroups,Dir,DirData) ->
    case httpd_util:split(DecodedString,":",2) of
	{ok, [SupposedUser, Password]} ->
	    case user_accepted(SupposedUser, ValidUsers) of
		true ->
		    check_password(SupposedUser, Password, Dir, DirData);
		false ->
		    case group_accepted(Info,SupposedUser,
					ValidGroups,Dir,DirData) of
			true ->
			    check_password(SupposedUser,Password,Dir,DirData);
			false ->
			    {no,?NICE("No such user exists")}
		    end
	    end;
	{ok, BadCredentials} ->
	    {status,{401,none,?NICE("Bad credentials "++BadCredentials)}}
    end.

user_accepted(_SupposedUser, undefined) ->
    false;
user_accepted(SupposedUser, ValidUsers) ->
    lists:member(SupposedUser, ValidUsers).


group_accepted(_Info, _User, undefined, _Dir, _DirData) ->
    false;
group_accepted(_Info, _User, [], _Dir, _DirData) ->
    false;
group_accepted(Info, User, [Group|Rest], Dir, DirData) ->
    Ret = int_list_group_members(Group, Dir, DirData),
    case Ret of
	{ok, UserList} ->
	    case lists:member(User, UserList) of
		true ->
		    true;
		false ->
		    group_accepted(Info, User, Rest, Dir, DirData)
	    end;
	_ ->
	    false
    end.

check_password(User, Password, _Dir, DirData) ->
    case int_get_user(DirData, User) of
	{ok, UStruct} ->
	    case UStruct#httpd_user.password of
		Password ->
		    %% FIXME
		    {yes, UStruct#httpd_user.username};
		_ ->
		    {no, "No such user"}   % Don't say 'Bad Password' !!!
	    end;
	_Other ->
	    {no, "No such user"}
    end.


%% Middle API. These functions call the appropriate authentication module.
int_get_user(DirData, User) ->    
    AuthMod = auth_mod_name(DirData), 
    apply(AuthMod, get_user, [DirData, User]).

int_list_group_members(Group, _Dir, DirData) ->
    AuthMod = auth_mod_name(DirData),
    apply(AuthMod, list_group_members, [DirData, Group]).

auth_mod_name(DirData) ->
    case proplists:get_value(auth_type, DirData, plain) of
	plain ->  mod_auth_plain;
	mnesia -> mod_auth_mnesia;
	dets ->	  mod_auth_dets
    end.

secretp(Path,ConfigDB) ->
    Directories = ets:match(ConfigDB,{directory, {'$1','_'}}),
    case secret_path(Path, Directories) of
	{yes,Directory} ->
	    {yes, {Directory,
		   lists:flatten(
		     ets:match(ConfigDB,{directory, {Directory,'$1'}}))}};
	no ->
	    no
    end.

secret_path(Path, Directories) ->
    secret_path(Path, httpd_util:uniq(lists:sort(Directories)),to_be_found).

secret_path(_Path, [], to_be_found) ->
    no;
secret_path(_Path, [], Directory) ->
    {yes, Directory};
secret_path(Path, [[NewDirectory] | Rest], Directory) ->
    case re:run(Path, NewDirectory, [{capture, first}]) of
	{match, _} when Directory =:= to_be_found ->
	    secret_path(Path, Rest, NewDirectory);
	{match, [{_, Length}]} when Length > length(Directory)->
	    secret_path(Path, Rest,NewDirectory);
	{match, _} ->
	    secret_path(Path, Rest, Directory);
	nomatch ->
	    secret_path(Path, Rest, Directory)
    end.

allow({_,RemoteAddr}, _SocketType, _Socket, DirectoryData) ->
    Hosts = proplists:get_value(allow_from, DirectoryData, all),
    case validate_addr(RemoteAddr, Hosts) of
	true ->
	    allowed;
	false ->
	    {not_allowed, ?NICE("Connection from your host is not allowed")}
    end.

validate_addr(_RemoteAddr, all) ->            % When called from 'allow'
    true;
validate_addr(_RemoteAddr, none) ->           % When called from 'deny'
    false;
validate_addr(_RemoteAddr, []) ->
    false;
validate_addr(RemoteAddr, [HostRegExp | Rest]) ->
    case re:run(RemoteAddr, HostRegExp, [{capture, none}]) of
	match ->
	    true;
	nomatch ->
	    validate_addr(RemoteAddr,Rest)
    end.

deny({_,RemoteAddr}, _SocketType, _Socket,DirectoryData) ->
    Hosts = proplists:get_value(deny_from, DirectoryData, none),
    case validate_addr(RemoteAddr,Hosts) of
	true ->
	    {denied, ?NICE("Connection from your host is not allowed")};
	false ->
	    not_denied
    end.    


directory_config_check(Directory, DirData) ->
    case proplists:get_value(auth_type, DirData) of
	plain ->
	    check_filename_present(Directory,auth_user_file,DirData),
	    check_filename_present(Directory,auth_group_file,DirData);
	_ ->
	    ok
    end.
check_filename_present(Dir,AuthFile,DirData) ->
    case proplists:get_value(AuthFile,DirData) of
	Name when is_list(Name) ->
	    ok;
	_ ->
	    throw({missing_auth_file, AuthFile, {directory, {Dir, DirData}}})
    end.

store_directory(Directory0, DirData0, ConfigList) ->
    Port = proplists:get_value(port, ConfigList),
    DirData = case proplists:get_value(bind_address, ConfigList) of
		  undefined ->
		    [{port, Port}|DirData0];
		Addr ->
		    [{port, Port},{bind_address,Addr}|DirData0]
	    end,
    Directory = 
	case filename:pathtype(Directory0) of
	    relative ->
		SR = proplists:get_value(server_root, ConfigList),
		filename:join(SR, Directory0);
	    _ ->
		Directory0
	end,
    AuthMod =
	case proplists:get_value(auth_type, DirData0) of
	    mnesia -> mod_auth_mnesia;
	    dets ->   mod_auth_dets;
	    plain ->  mod_auth_plain;
	    _ ->      no_module_at_all
	end,  
    case AuthMod of
	no_module_at_all ->
	    {ok, {directory, {Directory, DirData}}};
	_ ->
	    %% Check that there are a password or add a standard password: 
	    %% "NoPassword"
	    %% In this way a user must select to use a noPassword
	    Passwd = 
		case proplists:get_value(auth_access_password, DirData) of
		    undefined ->
			?NOPASSWORD;
		    PassW ->
			PassW
		end,
	    DirDataLast = lists:keydelete(auth_access_password,1,DirData), 
	    Server_root = proplists:get_value(server_root, ConfigList),
	    case catch AuthMod:store_directory_data(Directory, 
						    DirDataLast, 
						    Server_root) of
		ok ->
		    add_auth_password(Directory, Passwd, ConfigList),
		    {ok, {directory, {Directory, DirDataLast}}};
		{ok, NewDirData} ->
		    add_auth_password(Directory, Passwd, ConfigList),
		    {ok, {directory, {Directory, NewDirData}}};
		{error, Reason} ->
		    {error, Reason};
		Other ->
		    {error, Other}
	    end
    end.

add_auth_password(Dir, Pwd0, ConfigList) ->    
    Addr = proplists:get_value(bind_address, ConfigList),
    Port = proplists:get_value(port, ConfigList),
    Profile = proplists:get_value(profile, ConfigList, ?DEFAULT_PROFILE),
    mod_auth_server:start(Addr, Port, Profile),
    mod_auth_server:add_password(Addr, Port, Dir, Pwd0).
    
%% Opt = [{port, Port},
%%        {addr, Addr},
%%        {dir,  Dir},
%%        {authPassword, AuthPassword} | FunctionSpecificData]
get_options(Opt, mandatory)->    
    case proplists:get_value(port, Opt, undefined) of
	Port when is_integer(Port) ->
	    case proplists:get_value(dir, Opt, undefined) of
		Dir when is_list(Dir) ->
		    Addr = proplists:get_value(addr, Opt,
					       undefined),
		    AuthPwd = proplists:get_value(authPassword, Opt,
						  ?NOPASSWORD),
		    {Addr, Port, Dir, AuthPwd};
		_->
		    {error, bad_dir}
	    end;
	_ ->
	    {error, bad_dir}
    end;

%% FunctionSpecificData = {userData, UserData} | {password, Password}
get_options(Opt, userData)->
    case proplists:get_value(userData, Opt, undefined) of
	undefined ->
	    {error, no_userdata};
	UserData ->
	    case proplists:get_value(password, Opt, undefined) of
		undefined->
		    {error, no_password};
		Pwd ->
		    {UserData, Pwd}
	    end
    end.
