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
%%     $Id: mod_auth_server.erl,v 1.1 2008/12/17 09:53:35 mikpe Exp $
%%

-module(mod_auth_server).

-include("httpd.hrl").
%% -include("mod_auth.hrl").
-include("httpd_verbosity.hrl").

-behaviour(gen_server).


%% mod_auth exports
-export([start/2, stop/2,
	 add_password/4, update_password/5,
	 add_user/5, delete_user/5, get_user/5, list_users/4,
	 add_group_member/6, delete_group_member/6, list_group_members/5,
	 delete_group/5, list_groups/4]).

%% Management exports
-export([verbosity/3]).

%% gen_server exports
-export([start_link/3,
	 init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-record(state,{tab}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% External API                                                     %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start_link/3
%%
%% NOTE: This is called by httpd_misc_sup when the process is started
%%
start_link(Addr, Port, Verbosity)->
    ?vlog("start_link -> entry with"
	  "~n   Addr: ~p"
	  "~n   Port: ~p", [Addr, Port]),
    Name = make_name(Addr, Port),
    gen_server:start_link({local, Name}, ?MODULE, [Verbosity],
			  [{timeout, infinity}]).


%% start/2

start(Addr, Port)->
    ?vtrace("start -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p", [Addr, Port]),
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
           Verbosity = get(auth_verbosity),
           case (catch httpd_misc_sup:start_auth_server(Addr, Port,
                                                        Verbosity)) of
		{ok, Pid} ->
		    put(auth_server, Pid),
		    ok;
		{error, Reason} ->
		    exit({failed_start_auth_server, Reason});
		Error ->
		    exit({failed_start_auth_server, Error})
	    end;
	_ -> %% Already started...
	    ok
    end.


%% stop/2

stop(Addr, Port)->
    ?vtrace("stop -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p", [Addr, Port]),
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined -> %% Already stopped
	    ok;
	_ ->
           (catch httpd_misc_sup:stop_auth_server(Addr, Port))
    end.


%% verbosity/3

verbosity(Addr, Port, Verbosity) ->
    Name = make_name(Addr, Port),
    Req  = {verbosity, Verbosity},
    call(Name, Req).


%% add_password/4

add_password(Addr, Port, Dir, Password)->
    Name = make_name(Addr, Port),
    Req  = {add_password, Dir, Password},
    call(Name, Req).


%% update_password/6

update_password(Addr, Port, Dir, Old, New) when list(New) ->
    Name = make_name(Addr, Port),
    Req  = {update_password, Dir, Old, New},
    call(Name, Req).


%% add_user/5

add_user(Addr, Port, Dir, User, Password) ->
    Name = make_name(Addr, Port),
    Req  = {add_user, Addr, Port, Dir, User, Password},
    call(Name, Req).


%% delete_user/5

delete_user(Addr, Port, Dir, UserName, Password) ->
    Name = make_name(Addr, Port),
    Req  = {delete_user, Addr, Port, Dir, UserName, Password},
    call(Name, Req).


%% get_user/5

get_user(Addr, Port, Dir, UserName, Password) ->
    Name = make_name(Addr, Port),
    Req  = {get_user, Addr, Port, Dir, UserName, Password},
    call(Name, Req).


%% list_users/4

list_users(Addr, Port, Dir, Password) ->
    Name = make_name(Addr,Port),
    Req  = {list_users, Addr, Port, Dir, Password},
    call(Name, Req).


%% add_group_member/6

add_group_member(Addr, Port, Dir, GroupName, UserName, Password) ->
    Name = make_name(Addr,Port),
    Req  = {add_group_member, Addr, Port, Dir, GroupName, UserName, Password},
    call(Name, Req).


%% delete_group_member/6

delete_group_member(Addr, Port, Dir, GroupName, UserName, Password) ->
    Name = make_name(Addr,Port),
    Req  = {delete_group_member, Addr, Port, Dir, GroupName, UserName, Password},
    call(Name, Req).


%% list_group_members/4

list_group_members(Addr, Port, Dir, Group, Password) ->
    Name = make_name(Addr, Port),
    Req  = {list_group_members, Addr, Port, Dir, Group, Password},
    call(Name, Req).


%% delete_group/5

delete_group(Addr, Port, Dir, GroupName, Password) ->
    Name = make_name(Addr, Port),
    Req  = {delete_group, Addr, Port, Dir, GroupName, Password},
    call(Name, Req).


%% list_groups/4

list_groups(Addr, Port, Dir, Password) ->
    Name = make_name(Addr, Port),
    Req  = {list_groups, Addr, Port, Dir, Password},
    call(Name, Req).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Server call-back functions                                       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% init

init([undefined]) ->
    init([?default_verbosity]);

init([Verbosity]) ->
    put(sname,auth),
    put(verbosity,Verbosity),
    ?vlog("starting",[]),
    {ok,#state{tab = ets:new(auth_pwd,[set,protected])}}.


%% handle_call

%% Add a user
handle_call({add_user, Addr, Port, Dir, User, AuthPwd}, _From, State) ->
    Reply = api_call(Addr, Port, Dir, add_user, User, AuthPwd, State),
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
	    _From, State)->
    Reply = api_call(Addr, Port, Dir, delete_group_member, [Group, User],
		     AuthPwd, State),
    {reply, Reply, State};

%% List all users thats standalone users
handle_call({list_users, Addr, Port, Dir, AuthPwd}, _From, State)->
    Reply = api_call(Addr, Port, Dir, list_users, [], AuthPwd, State),
    {reply, Reply, State};

%% Delete a user
handle_call({delete_user, Addr, Port, Dir, User, AuthPwd}, _From, State)->
    Reply = api_call(Addr, Port, Dir, delete_user, [User], AuthPwd, State),
    {reply, Reply, State};

%% Delete a group
handle_call({delete_group, Addr, Port, Dir, Group, AuthPwd}, _From, State)->
    Reply = api_call(Addr, Port, Dir, delete_group, [Group], AuthPwd, State),
    {reply, Reply, State};

%% List the current groups
handle_call({list_groups, Addr, Port, Dir, AuthPwd}, _From, State)->
    Reply = api_call(Addr, Port, Dir, list_groups, [], AuthPwd, State),
    {reply, Reply, State};

%% List the members of the given group
handle_call({list_group_members, Addr, Port, Dir, Group, AuthPwd},
	    _From, State)->
    Reply = api_call(Addr, Port, Dir, list_group_members, [Group],
		     AuthPwd, State),
    {reply, Reply, State};


%% Add password for a directory
handle_call({add_password, Dir, Password}, _From, State)->
    Reply = do_add_password(Dir, Password, State),
    {reply, Reply, State};


%% Update the password for a directory

handle_call({update_password, Dir, Old, New},_From,State)->
    Reply =
	case getPassword(State, Dir) of
	    OldPwd when binary(OldPwd)->
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

handle_call(stop, _From, State)->
    {stop, normal, State};

handle_call({verbosity,Verbosity},_From,State)->
    OldVerbosity = put(verbosity,Verbosity),
    ?vlog("set verbosity:  ~p -> ~p",[Verbosity,OldVerbosity]),
    {reply,OldVerbosity,State}.

handle_info(Info,State)->
    {noreply,State}.

handle_cast(Request,State)->
    {noreply,State}.


terminate(Reason,State) ->
    ets:delete(State#state.tab),
    ok.


%% code_change({down, ToVsn}, State, Extra)
%%
code_change({down, _}, #state{tab = Tab}, downgrade_to_2_6_0) ->
    ?vlog("downgrade to 2.6.0", []),
    {ok, {state, Tab, undefined}};


%% code_change(FromVsn, State, Extra)
%%
code_change(_, {state, Tab, _}, upgrade_from_2_6_0) ->
    ?vlog("upgrade from 2.6.0", []),
    {ok, #state{tab = Tab}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% The functions that really changes the data in the database       %%
%% of users to different directories                                %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API gateway

api_call(Addr, Port, Dir, Func, Args,Password,State) ->
    case controlPassword(Password,State,Dir) of
	ok->
	    ConfigName = httpd_util:make_name("httpd_conf",Addr,Port),
	    case ets:match_object(ConfigName, {directory, Dir, '$1'}) of
		[{directory, Dir, DirData}] ->
		    AuthMod = auth_mod_name(DirData),
		    ?DEBUG("api_call -> call ~p:~p",[AuthMod,Func]),
		    Ret = (catch apply(AuthMod, Func, [DirData|Args])),
		    ?DEBUG("api_call -> Ret: ~p",[ret]),
		    Ret;
		O ->
		    ?DEBUG("api_call -> O: ~p",[O]),
		    {error, no_such_directory}
	    end;
	bad_password ->
	    {error,bad_password}
    end.

controlPassword(Password,State,Dir)when Password=:="DummyPassword"->
    bad_password;

controlPassword(Password,State,Dir)->
    case getPassword(State,Dir) of
	Pwd when binary(Pwd)->
	    case erlang:md5(Password) of
		Pwd ->
		    ok;
		_->
		    bad_password
	    end;
	_ ->
	    bad_password
    end.


getPassword(State,Dir)->
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
	PwdExists when binary(PwdExists) ->
	    {error, dir_protected};
	{error, _} ->
	    do_update_password(Dir, Password, State)
    end.


auth_mod_name(DirData) ->
    case httpd_util:key1search(DirData, auth_type, plain) of
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
