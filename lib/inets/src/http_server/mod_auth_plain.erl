%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(mod_auth_plain).

-include("httpd.hrl").
-include("mod_auth.hrl").
-include("httpd_internal.hrl").

-define(VMODULE,"AUTH_PLAIN").

%% Internal API
-export([store_directory_data/3]).
-export([get_user/2, 
	 list_group_members/2, 
	 add_user/2, 
	 add_group_member/3, 
	 list_users/1, 
	 delete_user/2, 
	 list_groups/1, 
	 delete_group_member/3, 
	 delete_group/2, 
	 remove/1]).

%%====================================================================
%% Internal application API
%%====================================================================	     

%% Storage format of users in the ets table:
%% {UserName, Password, UserData}
add_user(DirData, #httpd_user{username = User} = UStruct) ->
    PWDB = proplists:get_value(auth_user_file, DirData),
    Record = {User,
	      UStruct#httpd_user.password, 
	      UStruct#httpd_user.user_data}, 
    case ets:lookup(PWDB, User) of
	[{User, _SomePassword, _SomeData}] ->
	    {error, user_already_in_db};
	_ ->
	    ets:insert(PWDB, Record),
	    true
    end.

get_user(DirData, User) ->
    PWDB = proplists:get_value(auth_user_file, DirData),
    case ets:lookup(PWDB, User) of
	[{User, PassWd, Data}] ->
	    {ok, #httpd_user{username  = User, 
			     password  = PassWd, 
			     user_data = Data}};
	_Other ->
	    {error, no_such_user}
    end.

list_users(DirData) ->
    PWDB = proplists:get_value(auth_user_file, DirData),
    Records = ets:match(PWDB, '$1'), 
    {ok, lists:foldr(fun({User, _PassWd, _Data}, A) -> [User | A] end, 
		     [], lists:flatten(Records))}.

delete_user(DirData, UserName) ->
    PWDB = proplists:get_value(auth_user_file, DirData),
    case ets:lookup(PWDB, UserName) of
	[{UserName, _SomePassword, _SomeData}] ->
	    ets:delete(PWDB, UserName),
	    {ok, Groups}  = list_groups(DirData),
	    lists:foreach(fun(Group) -> 
				  delete_group_member(DirData, 
						      Group, UserName) 
			  end, Groups);
	_ ->
	    {error, no_such_user}
    end.

%% Storage of groups in the ets table:
%% {Group, UserList} where UserList is a list of strings.
add_group_member(DirData, Group, UserName) ->
    GDB = proplists:get_value(auth_group_file, DirData),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] ->
	    case lists:member(UserName, Users) of
		true ->
		    true;
		false ->
		    ets:insert(GDB, {Group, [UserName|Users]}),
		    true
	    end;
	[] ->
	    ets:insert(GDB, {Group, [UserName]}),
	    true;
	Other ->
	    {error, Other}
    end.

list_group_members(DirData, Group) ->
    GDB = proplists:get_value(auth_group_file, DirData),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] ->
	    {ok, Users};
	_ ->
	    {error, no_such_group}
    end.

list_groups(DirData) ->
    GDB = proplists:get_value(auth_group_file, DirData),
    Groups = ets:match(GDB, '$1'), 
    {ok, httpd_util:uniq(lists:foldr(fun({G, _}, A) -> [G|A] end,
				     [], lists:flatten(Groups)))}.

delete_group_member(DirData, Group, User) ->
    GDB = proplists:get_value(auth_group_file, DirData),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] when is_list(Users) ->
	    case lists:member(User, Users) of
		true ->
		    ets:delete(GDB, Group),
		    ets:insert(GDB, {Group, lists:delete(User, Users)}),
		    true;
		false ->
		    {error, no_such_group_member}
	    end;
	_ ->
	    {error, no_such_group}
    end.

delete_group(DirData, Group) ->
    GDB = proplists:get_value(auth_group_file, DirData),
    case ets:lookup(GDB, Group) of
	[{Group, _Users}] ->
	    ets:delete(GDB, Group),
	    true;
	_ ->
	    {error, no_such_group}
    end.

store_directory_data(_Directory, DirData, Server_root) ->
    PWFile = absolute_file_name(auth_user_file, DirData, Server_root),
    GroupFile = absolute_file_name(auth_group_file, DirData, Server_root),
    case load_passwd(PWFile) of
	{ok, PWDB} ->
	    case load_group(GroupFile) of
		{ok, GRDB} ->
		    %% Address and port is included in the file names...
		    Addr = proplists:get_value(bind_address, DirData),
		    Port = proplists:get_value(port, DirData),
		    {ok, PasswdDB} = store_passwd(Addr,Port,PWDB),
		    {ok, GroupDB}  = store_group(Addr,Port,GRDB),
		    NDD1 = lists:keyreplace(auth_user_file, 1, DirData, 
					    {auth_user_file, PasswdDB}),
		    NDD2 = lists:keyreplace(auth_group_file, 1, NDD1, 
					    {auth_group_file, GroupDB}),
		    {ok, NDD2};
		Err ->
		    {error, Err}
	    end;
	Err2 ->
	    {error, Err2}
    end.

%% Deletes ets tables used by this auth mod.
remove(DirData) ->
    PWDB = proplists:get_value(auth_user_file, DirData),
    GDB = proplists:get_value(auth_group_file, DirData),
    ets:delete(PWDB),
    ets:delete(GDB).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% Return the absolute path name of File_type. 
absolute_file_name(File_type, DirData, Server_root) ->
    Path = proplists:get_value(File_type, DirData),
    case filename:pathtype(Path) of
	relative ->
	    case Server_root of
		undefined ->
		    {error,
		     ?NICE(Path++
			   " is an invalid file name because "
			   "ServerRoot is not defined")};
		_ ->
		    filename:join(Server_root,Path)
	    end;
	_ ->
	    Path
    end.

store_group(Addr,Port,GroupList) ->
    %% Not a named table so not importante to add Profile to name
    Name = httpd_util:make_name("httpd_group",Addr,Port),
    GroupDB = ets:new(Name, [set, public]),
    store_group(GroupDB, GroupList).

store_group(GroupDB,[]) ->
    {ok, GroupDB};
store_group(GroupDB, [User|Rest]) ->
    ets:insert(GroupDB, User),
    store_group(GroupDB, Rest).

store_passwd(Addr,Port,PasswdList) ->
    %% Not a named table so not importante to add Profile to name
    Name = httpd_util:make_name("httpd_passwd",Addr,Port),
    PasswdDB = ets:new(Name, [set, public]),
    store_passwd(PasswdDB, PasswdList).

store_passwd(PasswdDB, []) ->
    {ok, PasswdDB};
store_passwd(PasswdDB, [User|Rest]) ->
    ets:insert(PasswdDB, User),
    store_passwd(PasswdDB, Rest).

parse_group(Stream, GroupList) ->
    Line =
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		httpd_conf:white_space_clean(String)
	end,
    parse_group(Stream, GroupList, Line).

parse_group(Stream, GroupList, eof) ->
    file:close(Stream),
    {ok, GroupList};
parse_group(Stream, GroupList, "") ->
    parse_group(Stream, GroupList);
parse_group(Stream, GroupList, [$#|_]) ->
    parse_group(Stream, GroupList);
parse_group(Stream, GroupList, Line) ->      
    case re:split(Line, ":", [{return, list}]) of
	[Group,Users] ->
	    UserList = re:split(Users," ", [{return, list}]),
	    parse_group(Stream, [{Group,UserList}|GroupList]);
	_ ->
	    {error, ?NICE(Line)}
    end.

load_passwd(AuthUserFile) ->
    case file:open(AuthUserFile, [read]) of
	{ok,Stream} ->
	    parse_passwd(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open " ++ AuthUserFile)}
    end.

parse_passwd(Stream, PasswdList) ->
    Line =
	case io:get_line(Stream, '') of
	    eof ->
		eof;
	    String ->
		httpd_conf:white_space_clean(String)
	end,
    parse_passwd(Stream, PasswdList, Line).

parse_passwd(Stream, PasswdList, eof) ->
    file:close(Stream),
    {ok, PasswdList};
parse_passwd(Stream, PasswdList, "") ->
    parse_passwd(Stream, PasswdList);
parse_passwd(Stream, PasswdList, [$#|_]) ->
    parse_passwd(Stream, PasswdList);
parse_passwd(Stream, PasswdList, Line) ->      
    case re:split(Line,":", [{return, list}]) of
	[User,Password] ->
	    parse_passwd(Stream, [{User,Password, []}|PasswdList]);
	_ ->
	    {error, ?NICE(Line)}
    end.

load_group(AuthGroupFile) ->
    case file:open(AuthGroupFile, [read]) of
	{ok, Stream} ->
	    parse_group(Stream,[]);
	{error, _} ->
	    {error, ?NICE("Can't open " ++ AuthGroupFile)}
    end.
