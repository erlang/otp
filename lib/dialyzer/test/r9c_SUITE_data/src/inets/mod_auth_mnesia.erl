%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mod_auth_mnesia.erl,v 1.2 2010/03/04 13:54:19 maria Exp $
%%
-module(mod_auth_mnesia).
-export([get_user/2,
	 list_group_members/2,
	 add_user/2,
	 add_group_member/3,
	 list_users/1,
	 delete_user/2,
	 list_groups/1,
	 delete_group_member/3,
	 delete_group/2]).

-export([store_user/5, store_user/6,
	 store_group_member/5, store_group_member/6,
	 list_group_members/3, list_group_members/4,
	 list_groups/2, list_groups/3,
	 list_users/2, list_users/3,
	 remove_user/4, remove_user/5,
	 remove_group_member/5, remove_group_member/6,
	 remove_group/4, remove_group/5]).

-export([store_directory_data/2]).

-include("httpd.hrl").
-include("mod_auth.hrl").



store_directory_data(Directory, DirData) ->
    %% We don't need to do anything here, we could ofcourse check that the appropriate
    %% mnesia tables has been created prior to starting the http server.
    ok.


%%
%% API
%%

%% Compatibility API


store_user(UserName, Password, Port, Dir, AccessPassword) ->
   %% AccessPassword is ignored - was not used in previous version
   DirData = [{path,Dir},{port,Port}],
   UStruct = #httpd_user{username = UserName,
			 password = Password},
   add_user(DirData, UStruct).

store_user(UserName, Password, Addr, Port, Dir, AccessPassword) ->
   %% AccessPassword is ignored - was not used in previous version
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   UStruct = #httpd_user{username = UserName,
			 password = Password},
   add_user(DirData, UStruct).

store_group_member(GroupName, UserName, Port, Dir, AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   add_group_member(DirData, GroupName, UserName).

store_group_member(GroupName, UserName, Addr, Port, Dir, AccessPassword) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   add_group_member(DirData, GroupName, UserName).

list_group_members(GroupName, Port, Dir) ->
   DirData = [{path,Dir},{port,Port}],
   list_group_members(DirData, GroupName).

list_group_members(GroupName, Addr, Port, Dir) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   list_group_members(DirData, GroupName).

list_groups(Port, Dir) ->
   DirData = [{path,Dir},{port,Port}],
   list_groups(DirData).

list_groups(Addr, Port, Dir) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   list_groups(DirData).

list_users(Port, Dir) ->
   DirData = [{path,Dir},{port,Port}],
   list_users(DirData).

list_users(Addr, Port, Dir) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   list_users(DirData).

remove_user(UserName, Port, Dir, _AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   delete_user(DirData, UserName).

remove_user(UserName, Addr, Port, Dir, _AccessPassword) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   delete_user(DirData, UserName).

remove_group_member(GroupName,UserName,Port,Dir,_AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   delete_group_member(DirData, GroupName, UserName).

remove_group_member(GroupName,UserName,Addr,Port,Dir,_AccessPassword) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   delete_group_member(DirData, GroupName, UserName).

remove_group(GroupName,Port,Dir,_AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   delete_group(DirData, GroupName).

remove_group(GroupName,Addr,Port,Dir,_AccessPassword) ->
   DirData = [{path,Dir},{bind_address,Addr},{port,Port}],
   delete_group(DirData, GroupName).

%%
%% Storage format of users in the mnesia table:
%% httpd_user records
%%

add_user(DirData, UStruct) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    UserName = UStruct#httpd_user.username,
    Password = UStruct#httpd_user.password,
    Data     = UStruct#httpd_user.user_data,
    User=#httpd_user{username={UserName,Addr,Port,Dir},
		     password=Password,
		     user_data=Data},
    case mnesia:transaction(fun() -> mnesia:write(User) end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

get_user(DirData, UserName) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:read({httpd_user,
						 {UserName,Addr,Port,Dir}})
			    end) of
	{aborted,Reason} ->
	    {error, Reason};
	{'atomic',[]} ->
	    {error, no_such_user};
	{'atomic', [Record]} when record(Record, httpd_user) ->
	    {ok, Record#httpd_user{username=UserName}};
	Other ->
	    {error, no_such_user}
    end.

list_users(DirData) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:match_object({httpd_user,
							 {'_',Addr,Port,Dir},'_','_'})
			    end) of
	{aborted,Reason} ->
	    {error,Reason};
	{'atomic',Users} ->
	    {ok,
	     lists:foldr(fun({httpd_user, {UserName, AnyAddr, AnyPort, AnyDir},
			      Password, Data}, Acc) ->
				 [UserName|Acc]
			 end,
			 [], Users)}
    end.

delete_user(DirData, UserName) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:delete({httpd_user,
						   {UserName,Addr,Port,Dir}})
			    end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

%%
%% Storage of groups in the mnesia table:
%% Multiple instances of {#httpd_group, User}
%%

add_group_member(DirData, GroupName, User) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    Group=#httpd_group{name={GroupName, Addr, Port, Dir}, userlist=User},
    case mnesia:transaction(fun() -> mnesia:write(Group) end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

list_group_members(DirData, GroupName) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:read({httpd_group,
						 {GroupName,Addr,Port,Dir}})
			    end) of
	{aborted, Reason} ->
	    {error,Reason};
	{'atomic', Members} ->
	    {ok,[UserName || {httpd_group,{AnyGroupName,AnyAddr,AnyPort,AnyDir},UserName} <- Members,
			     AnyGroupName == GroupName, AnyAddr == Addr,
			     AnyPort == Port, AnyDir == Dir]}
  end.

list_groups(DirData) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:match_object({httpd_group,
							 {'_',Addr,Port,Dir},'_'})
			    end) of
	{aborted, Reason} ->
	    {error, Reason};
	{'atomic', Groups} ->
	    GroupNames=
		[GroupName || {httpd_group,{GroupName,AnyAddr,AnyPort,AnyDir}, UserName} <- Groups,
			      AnyAddr == Addr, AnyPort == AnyPort, AnyDir == Dir],
	    {ok, httpd_util:uniq(lists:sort(GroupNames))}
    end.

delete_group_member(DirData, GroupName, UserName) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    Group = #httpd_group{name={GroupName, Addr, Port, Dir}, userlist=UserName},
    case mnesia:transaction(fun() -> mnesia:delete_object(Group) end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

%% THIS IS WRONG (?) !
%% Should first match out all httpd_group records for this group and then
%% do mnesia:delete on those. Or ?

delete_group(DirData, GroupName) ->
    {Addr, Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:delete({httpd_group,
						   {GroupName,Addr,Port,Dir}})
			    end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

%% Utility functions.

lookup_common(DirData) ->
    Dir = httpd_util:key1search(DirData, path),
    Port = httpd_util:key1search(DirData, port),
    Addr = httpd_util:key1search(DirData, bind_address),
    {Addr, Port, Dir}.
