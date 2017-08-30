%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Security Audit Functionality

%%
%% The gen_server code.
%%
%% A gen_server is needed in this module to take care of shared access to the
%% data file used to store failed and successful authentications aswell as 
%% user blocks.
%%
%% The storage model is a write-through model with both an ets and a dets 
%% table. Writes are done to both the ets and then the dets table, but reads 
%% are only done from the ets table.
%%
%% This approach also enables parallelism when using dets by returning the 
%% same dets table identifier when opening several files with the same 
%% physical location.
%%
%% NOTE: This could be implemented using a single dets table, as it is 
%%       possible to open a dets file with the ram_file flag, but this 
%%       would require periodical sync's to disk, and it would be hard 
%%       to decide when such an operation should occur.
%%


-module(mod_security_server).

-include("httpd.hrl").
-include("httpd_internal.hrl").

-behaviour(gen_server).


%% User API exports (called via mod_security)
-export([list_blocked_users/2, list_blocked_users/3, 
	 block_user/5, 
	 unblock_user/3, unblock_user/4,
	 list_auth_users/2, list_auth_users/3]).

%% Internal exports (for mod_security only)
-export([start/3, stop/2, stop/3,
	 new_table/4, delete_tables/3, 
	 store_failed_auth/6, store_successful_auth/5, 
	 check_blocked_user/6]).

%% gen_server exports
-export([start_link/3, init/1, 
	 handle_info/2, handle_call/3, handle_cast/2, 
	 terminate/2,
	 code_change/3]).

%%====================================================================
%% Internal application API
%%====================================================================	     

%% NOTE: This is called by httpd_misc_sup when the process is started
start_link(Addr, Port, Profile) ->
    Name = make_name(Addr, Port, Profile),
    gen_server:start_link({local, Name}, ?MODULE, [], [{timeout, infinity}]).

%% Called  by the mod_security module.
start(Addr, Port, Profile) ->
    Name = make_name(Addr, Port, Profile),
    case whereis(Name) of
	undefined ->
	   httpd_misc_sup:start_sec_server(Addr, Port, Profile);
	_ -> %% Already started...
	    ok
    end.

stop(Port, Profile) ->
    stop(undefined, Port, Profile).
stop(Addr, Port, Profile) ->
    Name = make_name(Addr, Port, Profile),
    case whereis(Name) of
	undefined ->
	    ok;
	_ ->
	    httpd_misc_sup:stop_sec_server(Addr, Port, Profile)
    end.

addr(undefined) ->
    any;
addr(Addr) ->
    Addr.

list_blocked_users(Addr, Port) ->
    list_blocked_users(Addr, Port, ?DEFAULT_PROFILE).
list_blocked_users(Addr, Port, Profile) when is_atom(Profile)->
    Name = make_name(Addr, Port, Profile),
    Req  = {list_blocked_users, addr(Addr), Port, Profile,'_'},
    call(Name, Req);
list_blocked_users(Addr, Port, Dir) ->
    list_blocked_users(Addr, Port, ?DEFAULT_PROFILE, Dir).
list_blocked_users(Addr, Port, Profile, Dir) ->
    Name = make_name(Addr, Port, Profile),
    Req  = {list_blocked_users, addr(Addr), Port, Profile, Dir},
    call(Name, Req).

block_user(User, Addr, Port, Dir, Time) ->
    block_user(User, Addr, Port, ?DEFAULT_PROFILE, Dir, Time).
block_user(User, Addr, Port, Profile, Dir, Time) ->
    Name = make_name(Addr, Port, Profile),
    Req  = {block_user, User, addr(Addr), Port, Profile, Dir, Time},
    call(Name, Req).

unblock_user(User, Addr, Port) ->
    unblock_user(User, Addr, Port, ?DEFAULT_PROFILE).
unblock_user(User, Addr, Port, Profile) when is_atom(Profile)->
    Name = make_name(Addr, Port, Profile),
    Req  = {unblock_user, User, addr(Addr), Port, Profile, '_'},
    call(Name, Req);
unblock_user(User, Addr, Port, Dir) ->
    unblock_user(User, Addr, Port, ?DEFAULT_PROFILE, Dir).
unblock_user(User, Addr, Port, Profile, Dir) ->
    Name = make_name(Addr, Port, Profile),
    Req  = {unblock_user, User, addr(Addr), Port, Profile, Dir},
    call(Name, Req).

list_auth_users(Addr, Port) ->
    list_auth_users(Addr, Port, ?DEFAULT_PROFILE).
list_auth_users(Addr, Port, Profile) when is_atom(Profile) ->
    Name = make_name(Addr, Port, Profile),
    Req  = {list_auth_users, addr(Addr), Port, Profile, '_'},
    call(Name, Req);
list_auth_users(Addr, Port, Dir) ->
    list_auth_users(Addr, Port, ?DEFAULT_PROFILE, Dir).
list_auth_users(Addr, Port, Profile, Dir) ->
    Name = make_name(Addr,Port, Profile),
    Req  = {list_auth_users, addr(Addr), Port, Profile, Dir}, 
    call(Name, Req).

new_table(Addr, Port, Profile, TabName) ->
    Name = make_name(Addr,Port, Profile),
    Req  = {new_table, addr(Addr), Port, Profile, TabName}, 
    call(Name, Req).

delete_tables(Addr, Port, Profile) ->
    Name = make_name(Addr, Port, Profile),
    case whereis(Name) of
	undefined ->
	    ok;
	_ ->
	    call(Name, delete_tables)
    end.

store_failed_auth(Info, Addr, Port, Profile, DecodedString, SDirData) ->
    Name = make_name(Addr, Port, Profile),
    Msg  = {store_failed_auth, Profile, [Info,DecodedString,SDirData]},
    cast(Name, Msg).

store_successful_auth(Addr, Port, Profile, User, SDirData) ->
    Name = make_name(Addr,Port, Profile),
    Msg  = {store_successful_auth, [User,Addr,Port, Profile, SDirData]}, 
    cast(Name, Msg).
 
check_blocked_user(Info, User, SDirData, Addr, Port, Profile) ->
    Name = make_name(Addr, Port, Profile),
    Req  = {check_blocked_user, Profile, [Info, User, SDirData]}, 
    call(Name, Req).

%%====================================================================
%% Behavior call backs
%%====================================================================	     
init(_) ->
    process_flag(trap_exit, true),
    {ok, []}.

handle_call(stop, _From, _Tables) ->
    {stop, normal, ok, []};

handle_call({block_user, User, Addr, Port, Profile, Dir, Time}, _From, Tables) ->
    Ret = block_user_int(User, Addr, Port, Profile, Dir, Time),
    {reply, Ret, Tables};

handle_call({list_blocked_users, Addr, Port, Profile, Dir}, _From, Tables) ->
    Blocked = list_blocked(Tables, Addr, Port, Profile, Dir, []),
    {reply, Blocked, Tables};

handle_call({unblock_user, User, Addr, Port, Profile, Dir}, _From, Tables) ->
    Ret = unblock_user_int(User, Addr, Port, Profile,Dir),
    {reply, Ret, Tables};

handle_call({list_auth_users, Addr, Port, Profile, Dir}, _From, Tables) ->
    Auth = list_auth(Tables, Addr, Port, Profile, Dir, []),
    {reply, Auth, Tables};

handle_call({new_table, Addr, Port, Profile, Name}, _From, Tables) ->
    case lists:keysearch(Name, 1, Tables) of
	{value, {Name, {Ets, Dets}}} ->
	    {reply, {ok, {Ets, Dets}}, Tables};
	false ->
	    TName = make_name(Addr,Port, Profile, length(Tables)),
	    case dets:open_file(TName, [{type, bag}, {file, Name}, 
					{repair, true}, 
					{access, read_write}]) of
		{ok, DFile} ->
		    ETS = ets:new(TName, [bag, private]),
		    sync_dets_to_ets(DFile, ETS),
		    NewTables = [{Name, {ETS, DFile}}|Tables],
		    {reply, {ok, {ETS, DFile}}, NewTables};
		{error, Err} ->
		    {reply, {error, {create_dets, Err}}, Tables}
	    end
    end;

handle_call(delete_tables, _From, Tables) ->
    lists:foreach(fun({_Name, {ETS, DETS}}) ->
			  dets:close(DETS),
			  ets:delete(ETS)
		  end, Tables),
    {reply, ok, []};

handle_call({check_blocked_user, Profile, [Info, User, SDirData]}, _From, Tables) ->
    {ETS, DETS} = proplists:get_value(data_file, SDirData),
    Dir = proplists:get_value(path, SDirData),
    Addr = proplists:get_value(bind_address, SDirData),
    Port = proplists:get_value(port, SDirData),
    CBModule = 
	proplists:get_value(callback_module, SDirData, no_module_at_all),
    Ret = 
	check_blocked_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, CBModule),
    {reply, Ret, Tables};

handle_call(_Request,_From,Tables) ->
    {reply,ok,Tables}.

handle_cast({store_failed_auth, _,[_, _, []]}, Tables) ->
    %% Some other authentication scheme than mod_auth (example mod_htacess)
    %% was the source for the authentication failure so we should ignor it!
    {noreply, Tables};
handle_cast({store_failed_auth, Profile, [Info, DecodedString, SDirData]}, Tables) ->
    {ETS, DETS} = proplists:get_value(data_file, SDirData),
    Dir  = proplists:get_value(path, SDirData),
    Addr = proplists:get_value(bind_address, SDirData),
    Port = proplists:get_value(port, SDirData),
    {ok, [User,Password]} = httpd_util:split(DecodedString,":",2),
    Seconds = universal_time(),
    Key = {User, Dir, Addr, Port, Profile},
    %% Event
    CBModule = proplists:get_value(callback_module, 
				     SDirData, no_module_at_all),
    auth_fail_event(CBModule,Addr,Port,Dir,User,Password),
    
    %% Find out if any of this user's other failed logins are too old to keep..
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	[] ->
	    no;
	List ->
	    ExpireTime = proplists:get_value(fail_expire_time, 
					     SDirData, 30)*60,
	    lists:map(fun({failed, {TheKey, LS, Gen}}) ->
			      Diff = Seconds-LS,
			      if
				  Diff > ExpireTime ->
				      ets:match_delete(ETS, 
						       {failed, 
							{TheKey, LS, Gen}}),
				      dets:match_delete(DETS, 
							{failed, 
							 {TheKey, LS, Gen}});
				  true ->
				      ok
			      end
		      end,
		      List)
    end,

    %% Insert the new failure..
    Generation = length(ets:match_object(ETS, {failed, {Key, '_', '_'}})),
    ets:insert(ETS, {failed, {Key, Seconds, Generation}}),
    dets:insert(DETS, {failed, {Key, Seconds, Generation}}),
    
    %% See if we should block this user..
    MaxRetries = proplists:get_value(max_retries, SDirData, 3),
    BlockTime = proplists:get_value(block_time, SDirData, 60),
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	List1 when length(List1) >= MaxRetries ->
	    %% Block this user until Future
	    Future = Seconds+BlockTime*60,
	    Reason = io_lib:format("Blocking user ~s from dir ~s "
				   "for ~p minutes", 
				   [User, Dir, BlockTime]),
	    mod_log:security_log(Info, lists:flatten(Reason)),
	    
	    %% Event
	    user_block_event(CBModule,Addr,Port,Dir,User),
	    
	    ets:match_delete(ETS,{blocked_user,
				  {User, Addr, Port, Dir, '$1'}}), 
	    dets:match_delete(DETS, {blocked_user,
				     {User, Addr, Port, Dir, '$1'}}),
	    BlockRecord = {blocked_user, 
			   {User, Addr, Port, Profile, Dir, Future}},
	    ets:insert(ETS, BlockRecord),
	    dets:insert(DETS, BlockRecord),
	    %% Remove previous failed requests.
	    ets:match_delete(ETS, {failed, {Key, '_', '_'}}),
	    dets:match_delete(DETS, {failed, {Key, '_', '_'}});
	_ ->
	    no
    end,
    {noreply, Tables};

handle_cast({store_successful_auth, [User, Addr, Port, Profile, SDirData]}, Tables) ->
    {ETS, DETS} = proplists:get_value(data_file, SDirData),
    AuthTimeOut = proplists:get_value(auth_timeout, SDirData, 30),
    Dir = proplists:get_value(path, SDirData),
    Key = {User, Dir, Addr, Port, Profile},

    %% Remove failed entries for this Key
    dets:match_delete(DETS, {failed, {Key, '_', '_'}}),
    ets:match_delete(ETS, {failed, {Key, '_', '_'}}), 

    %% Keep track of when the last successful login took place.
    Seconds = universal_time()+AuthTimeOut,
    ets:match_delete(ETS, {success, {Key, '_'}}),
    dets:match_delete(DETS, {success, {Key, '_'}}),
    ets:insert(ETS, {success, {Key, Seconds}}),
    dets:insert(DETS, {success, {Key, Seconds}}),
    {noreply, Tables};
	    
handle_cast(Req, Tables) ->
    error_msg("security server got unknown cast: ~p",[Req]),
    {noreply, Tables}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _Tables) ->
    ok.

code_change(_, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% block_user_int/5
block_user_int(User, Addr, Port, Profile, Dir, Time) ->
    Dirs = httpd_manager:config_match(Addr, Port, Profile, 
				      {security_directory, {'_', '_'}}),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    Time1 = 
		case Time of
		    infinity ->
			99999999999999999999999999999;
		    _ ->
			Time
		end,
	    Future = universal_time()+Time1,
	    ets:match_delete(ETS, {blocked_user, {User,Addr,Port,Profile, Dir,'_'}}),
	    dets:match_delete(DETS, {blocked_user, 
				     {User,Addr,Port,Profile, Dir,'_'}}),
	    ets:insert(ETS, {blocked_user, {User,Addr,Port, Profile, Dir,Future}}),
	    dets:insert(DETS, {blocked_user, {User,Addr,Port,Profile, Dir,Future}}),
	    CBModule = proplists:get_value(callback_module, DirData, 
					     no_module_at_all),
	    user_block_event(CBModule,Addr,Port,Dir,User),
	    true;
	_ ->
	    {error, no_such_directory}
    end.
    
find_dirdata([], _Dir) ->
    false;
find_dirdata([{security_directory, {_, DirData}}|SDirs], Dir) ->
    case lists:keysearch(path, 1, DirData) of
	{value, {path, Dir}} ->
	    {value, {data_file, {ETS, DETS}}} =
		lists:keysearch(data_file, 1, DirData),
	    {ok, DirData, {ETS, DETS}};
	_ ->
	    find_dirdata(SDirs, Dir)
    end.

unblock_user_int(User, Addr, Port, Profile, Dir) ->
    Dirs = httpd_manager:config_match(Addr, Port, Profile, 
				      {security_directory, {'_', '_'}}),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    case ets:match_object(ETS,
				  {blocked_user,{User,Addr,Port,Profile,Dir,'_'}}) of
		[] ->
		    {error, not_blocked};
		_Objects ->
		    ets:match_delete(ETS, {blocked_user,
					   {User, Addr, Port, Profile, Dir, '_'}}),
		    dets:match_delete(DETS, {blocked_user,
					     {User, Addr, Port, Profile, Dir, '_'}}),
	       	    CBModule = proplists:get_value(callback_module, 
						     DirData, 
						     no_module_at_all),
		    user_unblock_event(CBModule,Addr,Port,Dir,User),
		    true
	    end;
	_ ->
	    {error, no_such_directory}
    end.

list_auth([], _, _, _, _, Acc) ->
    Acc;
list_auth([{_Name, {ETS, DETS}}|Tables], Addr, Port, Profile, Dir, Acc) ->
    case ets:match_object(ETS, {success, {{'_', Dir, Addr, Port, Profile}, '_'}}) of
	[] ->
	    list_auth(Tables, Addr, Port, Profile, Dir, Acc);
	List ->
	    TN = universal_time(),
	    NewAcc = lists:foldr(fun({success,{{U,Ad,P, Pr,D},T}},Ac) -> 
					 if
					     T-TN > 0 ->
						 [U|Ac];
					     true ->
						 Rec = {success,
							{{U,Ad,P,Pr,D},T}},
						 ets:match_delete(ETS,Rec),
						 dets:match_delete(DETS,Rec),
						 Ac
					 end
				 end,
				 Acc, List),
	    list_auth(Tables, Addr, Port, Profile, Dir, NewAcc)
    end.

list_blocked([], _, _, _, _, Acc) ->
    TN = universal_time(),
    lists:foldl(fun({U,Ad,P,Pr,D,T}, Ac) ->
			if
			    T-TN > 0 ->
				[{U,Ad,P, Pr,D,local_time(T)}|Ac];
			    true ->
				Ac
			end
		end, 
		[], Acc);
list_blocked([{_Name, {ETS, _DETS}}|Tables], Addr, Port, Profile, Dir, Acc) ->
    List = ets:match_object(ETS, {blocked_user, 
				  {'_',Addr,Port,Profile, Dir,'_'}}),
    
    NewBlocked = lists:foldl(fun({blocked_user, X}, A) -> 
				     [X|A] end, Acc, List),
    
    list_blocked(Tables, Addr, Port, Profile, Dir, NewBlocked).


%% Reads dets-table DETS and syncronizes it with the ets-table ETS.
%%
sync_dets_to_ets(DETS, ETS) ->
    dets:traverse(DETS, fun(X) ->
				ets:insert(ETS, X),
				continue
			end).

%% Check if a specific user is blocked from access.
%%
%% The sideeffect of this routine is that it unblocks also other users
%% whos blocking time has expired. This to keep the tables as small
%% as possible.
%%
check_blocked_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, CBModule) ->
    TN = universal_time(),
    BlockList = ets:match_object(ETS, {blocked_user, {User, '_', '_', '_', '_', '_'}}), 
    Blocked = lists:foldl(fun({blocked_user, X}, A) ->
				  [X|A] end, [], BlockList),
    check_blocked_user(Info,User,Dir,
		       Addr,Port, Profile, ETS,DETS,TN,Blocked,CBModule).

check_blocked_user(_Info, _, _, _, _, _, _, _, _,[], _CBModule) ->
    false;
check_blocked_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, TN, 
		   [{User,Addr,Port,Profile, Dir,T}| _], CBModule) ->
    TD = T-TN,
    if
	TD =< 0 ->
	    %% Blocking has expired, remove and grant access.
	    unblock_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, CBModule),
	    false;
	true ->
	    true
    end;
check_blocked_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, TN, 
		   [{OUser,ODir,OAddr,OPort, OProfile, T}|Ls], CBModule) ->
    TD = T-TN,
    if
	TD =< 0 ->
	    %% Blocking has expired, remove.
	    unblock_user(Info, OUser, ODir, OAddr, OPort, OProfile,
			 ETS, DETS, CBModule);
	true ->
	    true
    end,
    check_blocked_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, 
		       TN, Ls, CBModule).

unblock_user(Info, User, Dir, Addr, Port, Profile, ETS, DETS, CBModule) ->
    Reason =
	io_lib:format("User ~s was removed from the block list for dir ~s",
			 [User, Dir]),
    mod_log:security_log(Info, lists:flatten(Reason)),
    user_unblock_event(CBModule,Addr,Port,Dir,User),
    dets:match_delete(DETS, {blocked_user, {User, Addr, Port, Profile, Dir, '_'}}),
    ets:match_delete(ETS, {blocked_user, {User, Addr, Port, Profile, Dir, '_'}}).
  
make_name(Addr,Port, Profile) ->
    httpd_util:make_name(?MODULE_STRING, Addr, Port, Profile).

make_name(Addr,Port, Profile, Num) ->
    httpd_util:make_name(?MODULE_STRING, Addr,Port,
			 atom_to_list(Profile) ++ "__" ++ integer_to_list(Num)).

auth_fail_event(Mod,Addr,Port,Dir,User,Passwd) ->
    event(auth_fail,Mod,Addr,Port,Dir,[{user,User},{password,Passwd}]).

user_block_event(Mod,Addr,Port,Dir,User) ->
    event(user_block,Mod,Addr,Port,Dir,[{user,User}]).

user_unblock_event(Mod,Addr,Port,Dir,User) ->
    event(user_unblock,Mod,Addr,Port,Dir,[{user,User}]).

event(Event, Mod, undefined, Port, Dir, Info) ->
    (catch Mod:event(Event,Port,Dir,Info));
event(Event, Mod, any, Port, Dir, Info) ->
    (catch Mod:event(Event,Port,Dir,Info));
event(Event, Mod, Addr, Port, Dir, Info) ->
    (catch Mod:event(Event,Addr,Port,Dir,Info)).

universal_time() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

local_time(T) ->
    calendar:universal_time_to_local_time(
      calendar:gregorian_seconds_to_datetime(T)).

error_msg(F, A) ->
    error_logger:error_msg(F, A).

call(Name, Req) ->
    case (catch gen_server:call(Name, Req)) of
        {'EXIT', Reason} ->
            {error, Reason};
        Reply ->
            Reply
    end.

cast(Name, Msg) ->
    case (catch gen_server:cast(Name, Msg)) of
        {'EXIT', Reason} ->
            {error, Reason};
        Result ->
            Result
    end.
