%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
%%

-module(mod_auth_server).

-include("httpd.hrl").
-include("httpd_internal.hrl").

-behaviour(gen_server).


%% mod_auth exports 
-export([start/2, stop/2, 
	 add_password/4, update_password/5, 
	 add_user/5, delete_user/5, get_user/5, list_users/4, 
	 add_group_member/6, delete_group_member/6, list_group_members/5, 
	 delete_group/5, list_groups/4]).

%% gen_server exports
-export([start_link/2, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {tab}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% External API                                                     %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start_link/3
%% 
%% NOTE: This is called by httpd_misc_sup when the process is started
%% 
start_link(Addr, Port) ->
    ?hdrt("start_link", [{address, Addr}, {port, Port}]),
    Name = make_name(Addr, Port),
    gen_server:start_link({local, Name}, ?MODULE, [], [{timeout, infinity}]).


%% start/2

start(Addr, Port) ->
    ?hdrd("start", [{address, Addr}, {port, Port}]),
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
	    httpd_misc_sup:start_auth_server(Addr, Port);
	_ -> %% Already started...
	    ok
    end.


%% stop/2

stop(Addr, Port) ->
    ?hdrd("stop", [{address, Addr}, {port, Port}]),
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined -> %% Already stopped
	    ok;
	_ ->
           (catch httpd_misc_sup:stop_auth_server(Addr, Port))
    end.

%% add_password/4

add_password(Addr, Port, Dir, Password) ->
    ?hdrt("add password", [{address, Addr}, {port, Port}]),
    Name = make_name(Addr, Port),
    Req  = {add_password, Dir, Password},
    call(Name, Req).


%% update_password/6

update_password(Addr, Port, Dir, Old, New) when is_list(New) ->
    ?hdrt("update password", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, {old, Old}, {new, New}]),
    Name = make_name(Addr, Port),
    Req  = {update_password, Dir, Old, New},
    call(Name, Req).
	   
 
%% add_user/5

add_user(Addr, Port, Dir, User, Password) ->
    ?hdrt("add user", 
	  [{address, Addr}, {port, Port}, 
	   {dir, Dir}, {user, User}, {passwd, Password}]),
    Name = make_name(Addr, Port),
    Req  = {add_user, Addr, Port, Dir, User, Password},
    call(Name, Req).


%% delete_user/5

delete_user(Addr, Port, Dir, UserName, Password) ->
    ?hdrt("delete user", 
	  [{address, Addr}, {port, Port}, 
	   {dir, Dir}, {user, UserName}, {passwd, Password}]),
    Name = make_name(Addr, Port),
    Req  = {delete_user, Addr, Port, Dir, UserName, Password},
    call(Name, Req).


%% get_user/5

get_user(Addr, Port, Dir, UserName, Password) ->
    ?hdrt("get user", 
	  [{address, Addr}, {port, Port}, 
	   {dir, Dir}, {user, UserName}, {passwd, Password}]),
    Name = make_name(Addr, Port),
    Req  = {get_user, Addr, Port, Dir, UserName, Password},
    call(Name, Req).


%% list_users/4

list_users(Addr, Port, Dir, Password) ->
    ?hdrt("list users", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, {passwd, Password}]),
    Name = make_name(Addr,Port),
    Req  = {list_users, Addr, Port, Dir, Password},
    call(Name, Req).


%% add_group_member/6

add_group_member(Addr, Port, Dir, GroupName, UserName, Password) ->
    ?hdrt("add group member", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, 
	   {group, GroupName}, {user, UserName}, {passwd, Password}]),
    Name = make_name(Addr,Port),
    Req  = {add_group_member, Addr, Port, Dir, GroupName, UserName, Password},
    call(Name, Req).


%% delete_group_member/6

delete_group_member(Addr, Port, Dir, GroupName, UserName, Password) ->
    ?hdrt("delete group member", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, 
	   {group, GroupName}, {user, UserName}, {passwd, Password}]),
    Name = make_name(Addr,Port),
    Req  = {delete_group_member, Addr, Port, Dir, GroupName, UserName, Password},
    call(Name, Req).


%% list_group_members/4

list_group_members(Addr, Port, Dir, Group, Password) ->
    ?hdrt("list group members", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, 
	   {group, Group}, {passwd, Password}]),
    Name = make_name(Addr, Port),
    Req  = {list_group_members, Addr, Port, Dir, Group, Password},
    call(Name, Req).


%% delete_group/5

delete_group(Addr, Port, Dir, GroupName, Password) ->
    ?hdrt("delete group", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, 
	   {group, GroupName}, {passwd, Password}]),
    Name = make_name(Addr, Port),
    Req  = {delete_group, Addr, Port, Dir, GroupName, Password},
    call(Name, Req).


%% list_groups/4

list_groups(Addr, Port, Dir, Password) ->
    ?hdrt("list groups", 
	  [{address, Addr}, {port, Port}, {dir, Dir}, {passwd, Password}]),
    Name = make_name(Addr, Port),
    Req  = {list_groups, Addr, Port, Dir, Password},
    call(Name, Req).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Server call-back functions                                       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% init

init(_) ->
    ?hdrv("initiating", []),
    {ok,#state{tab = ets:new(auth_pwd,[set,protected])}}.

%% handle_call

%% Add a user
handle_call({add_user, Addr, Port, Dir, User, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, add_user, User, AuthPwd, State),
    ?hdrt("add user", [{reply, Reply}]),
    {reply, Reply, State};

%% Get data about a user
handle_call({get_user, Addr, Port, Dir, User, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, get_user, [User], AuthPwd, State),
    {reply, Reply, State};

%% Add a group member
handle_call({add_group_member, Addr, Port, Dir, Group, User, AuthPwd},
	    _From, State) ->
    Reply = api_call(Addr, Port, Dir, add_group_member, [Group, User], 
		     AuthPwd, State),
    {reply, Reply, State};

%% delete a group
handle_call({delete_group_member, Addr, Port, Dir, Group, User, AuthPwd},
	    _From, State) ->
    Reply = api_call(Addr, Port, Dir, delete_group_member, [Group, User], 
		     AuthPwd, State), 
    {reply, Reply, State};

%% List all users thats standalone users
handle_call({list_users, Addr, Port, Dir, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, list_users, [], AuthPwd, State),
    {reply, Reply, State};

%% Delete a user
handle_call({delete_user, Addr, Port, Dir, User, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, delete_user, [User], AuthPwd, State),
    {reply, Reply, State};

%% Delete a group
handle_call({delete_group, Addr, Port, Dir, Group, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, delete_group, [Group], AuthPwd, State),
    {reply, Reply, State};

%% List the current groups
handle_call({list_groups, Addr, Port, Dir, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, list_groups, [], AuthPwd, State),
    {reply, Reply, State};

%% List the members of the given group
handle_call({list_group_members, Addr, Port, Dir, Group, AuthPwd},
	    _From, State) ->
    Reply = api_call(Addr, Port, Dir, list_group_members, [Group],
		     AuthPwd, State), 
    {reply, Reply, State};


%% Add password for a directory
handle_call({add_password, Dir, Password}, _From, State) ->
    Reply = do_add_password(Dir, Password, State),
    {reply, Reply, State};


%% Update the password for a directory
  
handle_call({update_password, Dir, Old, New},_From,State) ->
    Reply = 
	case getPassword(State, Dir) of
	    OldPwd when is_binary(OldPwd) ->
		case erlang:md5(Old) of
		    OldPwd ->
			%% The old password is right =>
			%% update the password to the new
			do_update_password(Dir,New,State),
			ok;
		_->
		    {error, error_new}
	    end;
	_->
	    {error, error_old}
    end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
    

terminate(_Reason,State) ->
    ets:delete(State#state.tab),
    ok.


%% code_change(Vsn, State, Extra)
%%
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% The functions that really changes the data in the database       %%
%% of users to different directories                                %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% API gateway

api_call(Addr, Port, Dir, Func, Args,Password,State) ->
    case controlPassword(Password, State, Dir) of
	ok->
	    ConfigName = httpd_util:make_name("httpd_conf", Addr, Port),
	    case ets:match_object(ConfigName, {directory, {Dir, '$1'}}) of
		[{directory, {Dir, DirData}}] ->
		    AuthMod = auth_mod_name(DirData),
		    (catch apply(AuthMod, Func, [DirData|Args]));
		_ ->
		    {error, no_such_directory}
	    end;
	bad_password ->
	    {error,bad_password}
    end.

controlPassword(Password, _State, _Dir) when Password =:= "DummyPassword" ->
    bad_password;

controlPassword(Password,State,Dir) ->
    case getPassword(State,Dir) of
	Pwd when is_binary(Pwd) ->
	    case erlang:md5(Password) of
		Pwd ->
		    ok;
		_->
		    bad_password
	    end;
	_ ->
	    bad_password
    end.

    
getPassword(State, Dir) ->
    case lookup(State#state.tab, Dir) of
	[{_,Pwd}]->
	    Pwd;
	_ ->
	    {error,bad_password}
    end.

do_update_password(Dir, New, State) ->
    ets:insert(State#state.tab, {Dir, erlang:md5(New)}).

do_add_password(Dir, Password, State) ->
    case getPassword(State,Dir) of
	PwdExists when is_binary(PwdExists) ->
	    {error, dir_protected};
	{error, _} ->
	    do_update_password(Dir, Password, State)
    end.
	    

auth_mod_name(DirData) ->
    case proplists:get_value(auth_type, DirData, plain) of
	plain ->    mod_auth_plain;
	mnesia ->   mod_auth_mnesia;
	dets ->	    mod_auth_dets
    end.

    
lookup(Db, Key) ->
    ets:lookup(Db, Key).


make_name(Addr,Port) ->
    httpd_util:make_name("httpd_auth",Addr,Port).


call(Name, Req) ->
    case (catch gen_server:call(Name, Req)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Reply ->
	    Reply
    end.
    

