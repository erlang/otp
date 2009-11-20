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
-export([start/2, stop/1, stop/2,
	 new_table/3, delete_tables/2, 
	 store_failed_auth/5, store_successful_auth/4, 
	 check_blocked_user/5]).

%% gen_server exports
-export([start_link/2, init/1, 
	 handle_info/2, handle_call/3, handle_cast/2, 
	 terminate/2,
	 code_change/3]).


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
%% Called  by the mod_security module.

start(Addr, Port) ->
    ?hdrt("start", [{address, Addr}, {port, Port}]),
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
	   httpd_misc_sup:start_sec_server(Addr, Port);
	_ -> %% Already started...
	    ok
    end.


%% stop

stop(Port) ->
    stop(undefined, Port).
stop(Addr, Port) ->
    ?hdrt("stop", [{address, Addr}, {port, Port}]),
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
	    ok;
	_ ->
	    httpd_misc_sup:stop_sec_server(Addr, Port)
    end.


addr(undefined) ->
    any;
addr(Addr) ->
    Addr.


%% list_blocked_users

list_blocked_users(Addr, Port) ->
    Name = make_name(Addr, Port),
    Req  = {list_blocked_users, addr(Addr), Port, '_'},
    call(Name, Req).

list_blocked_users(Addr, Port, Dir) ->
    Name = make_name(Addr, Port),
    Req  = {list_blocked_users, addr(Addr), Port, Dir},
    call(Name, Req).


%% block_user

block_user(User, Addr, Port, Dir, Time) ->
    Name = make_name(Addr, Port),
    Req  = {block_user, User, addr(Addr), Port, Dir, Time},
    call(Name, Req).


%% unblock_user

unblock_user(User, Addr, Port) ->
    Name = make_name(Addr, Port),
    Req  = {unblock_user, User, addr(Addr), Port, '_'},
    call(Name, Req).

unblock_user(User, Addr, Port, Dir) ->
    Name = make_name(Addr, Port),
    Req  = {unblock_user, User, addr(Addr), Port, Dir},
    call(Name, Req).


%% list_auth_users

list_auth_users(Addr, Port) ->
    Name = make_name(Addr, Port),
    Req  = {list_auth_users, addr(Addr), Port, '_'},
    call(Name, Req).

list_auth_users(Addr, Port, Dir) ->
    Name = make_name(Addr,Port),
    Req  = {list_auth_users, addr(Addr), Port, Dir}, 
    call(Name, Req).
    

%% new_table

new_table(Addr, Port, TabName) ->
    Name = make_name(Addr,Port),
    Req  = {new_table, addr(Addr), Port, TabName}, 
    call(Name, Req).


%% delete_tables
    
delete_tables(Addr, Port) ->
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
	    ok;
	_ ->
	    call(Name, delete_tables)
    end.


%% store_failed_auth

store_failed_auth(Info, Addr, Port, DecodedString, SDirData) ->
    ?hdrv("store failed auth", 
	  [{addr, Addr}, {port, Port}, 
	   {decoded_string, DecodedString}, {sdir_data, SDirData}]),
    Name = make_name(Addr,Port),
    Msg  = {store_failed_auth,[Info,DecodedString,SDirData]},
    cast(Name, Msg).


%% store_successful_auth

store_successful_auth(Addr, Port, User, SDirData) ->
    Name = make_name(Addr,Port),
    Msg  = {store_successful_auth, [User,Addr,Port,SDirData]}, 
    cast(Name, Msg).
    

%% check_blocked_user

check_blocked_user(Info, User, SDirData, Addr, Port) ->
    Name = make_name(Addr, Port),
    Req  = {check_blocked_user, [Info, User, SDirData]}, 
    call(Name, Req).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Server call-back functions                                       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    ?hdrv("initiating", []),
    process_flag(trap_exit, true),
    {ok, []}.

handle_call(stop, _From, _Tables) ->
    {stop, normal, ok, []};

handle_call({block_user, User, Addr, Port, Dir, Time}, _From, Tables) ->
    ?hdrv("block user", 
	  [{user, User}, {addr, Addr}, {port, Port}, {dir, Dir}, 
	   {time, Time}]), 
    Ret = block_user_int(User, Addr, Port, Dir, Time),
    {reply, Ret, Tables};

handle_call({list_blocked_users, Addr, Port, Dir}, _From, Tables) ->
    ?hdrv("list blocked users", 
	  [{addr, Addr}, {port, Port}, {dir, Dir}]),
    Blocked = list_blocked(Tables, Addr, Port, Dir, []),
    {reply, Blocked, Tables};

handle_call({unblock_user, User, Addr, Port, Dir}, _From, Tables) ->
    ?hdrv("block user", 
	  [{user, User}, {addr, Addr}, {port, Port}, {dir, Dir}]), 
    Ret = unblock_user_int(User, Addr, Port, Dir),
    {reply, Ret, Tables};

handle_call({list_auth_users, Addr, Port, Dir}, _From, Tables) ->
    ?hdrv("list auth users", 
	  [{addr, Addr}, {port, Port}, {dir, Dir}]), 
    Auth = list_auth(Tables, Addr, Port, Dir, []),
    {reply, Auth, Tables};

handle_call({new_table, Addr, Port, Name}, _From, Tables) ->
    case lists:keysearch(Name, 1, Tables) of
	{value, {Name, {Ets, Dets}}} ->
	    {reply, {ok, {Ets, Dets}}, Tables};
	false ->
	    TName = make_name(Addr,Port,length(Tables)),
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

handle_call({check_blocked_user, [Info, User, SDirData]}, _From, Tables) ->
    {ETS, DETS} = proplists:get_value(data_file, SDirData),
    Dir = proplists:get_value(path, SDirData),
    Addr = proplists:get_value(bind_address, SDirData),
    Port = proplists:get_value(port, SDirData),
    CBModule = 
	proplists:get_value(callback_module, SDirData, no_module_at_all),
    Ret = 
	check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule),
    {reply, Ret, Tables};

handle_call(_Request,_From,Tables) ->
    {reply,ok,Tables}.


%% handle_cast

handle_cast({store_failed_auth, [_, _, []]}, Tables) ->
    %% Some other authentication scheme than mod_auth (example mod_htacess)
    %% was the source for the authentication failure so we should ignor it!
    {noreply, Tables};
handle_cast({store_failed_auth, [Info, DecodedString, SDirData]}, Tables) ->
    {ETS, DETS} = proplists:get_value(data_file, SDirData),
    Dir  = proplists:get_value(path, SDirData),
    Addr = proplists:get_value(bind_address, SDirData),
    Port = proplists:get_value(port, SDirData),
    {ok, [User,Password]} = httpd_util:split(DecodedString,":",2),
    Seconds = universal_time(),
    Key = {User, Dir, Addr, Port},
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
			   {User, Addr, Port, Dir, Future}},
	    ets:insert(ETS, BlockRecord),
	    dets:insert(DETS, BlockRecord),
	    %% Remove previous failed requests.
	    ets:match_delete(ETS, {failed, {Key, '_', '_'}}),
	    dets:match_delete(DETS, {failed, {Key, '_', '_'}});
	_ ->
	    no
    end,
    {noreply, Tables};

handle_cast({store_successful_auth, [User, Addr, Port, SDirData]}, Tables) ->
    {ETS, DETS} = proplists:get_value(data_file, SDirData),
    AuthTimeOut = proplists:get_value(auth_timeout, SDirData, 30),
    Dir = proplists:get_value(path, SDirData),
    Key = {User, Dir, Addr, Port},

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


%% handle_info

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate

terminate(_Reason, _Tables) ->
    ok.


%% code_change({down, ToVsn}, State, Extra)
%% 
code_change({down, _}, State, _Extra) ->
    {ok, State};


%% code_change(FromVsn, State, Extra)
%%
code_change(_, State, _Extra) ->
    {ok, State}.

%% block_user_int/5
block_user_int(User, Addr, Port, Dir, Time) ->
    Dirs = httpd_manager:config_match(Addr, Port, 
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
	    ets:match_delete(ETS, {blocked_user, {User,Addr,Port,Dir,'_'}}),
	    dets:match_delete(DETS, {blocked_user, 
				     {User,Addr,Port,Dir,'_'}}),
	    ets:insert(ETS, {blocked_user, {User,Addr,Port,Dir,Future}}),
	    dets:insert(DETS, {blocked_user, {User,Addr,Port,Dir,Future}}),
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

%% unblock_user_int/4
unblock_user_int(User, Addr, Port, Dir) ->
    Dirs = httpd_manager:config_match(Addr, Port, 
				      {security_directory, {'_', '_'}}),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    case ets:match_object(ETS,
				  {blocked_user,{User,Addr,Port,Dir,'_'}}) of
		[] ->
		    {error, not_blocked};
		_Objects ->
		    ets:match_delete(ETS, {blocked_user,
					   {User, Addr, Port, Dir, '_'}}),
		    dets:match_delete(DETS, {blocked_user,
					     {User, Addr, Port, Dir, '_'}}),
	       	    CBModule = proplists:get_value(callback_module, 
						     DirData, 
						     no_module_at_all),
		    user_unblock_event(CBModule,Addr,Port,Dir,User),
		    true
	    end;
	_ ->
	    {error, no_such_directory}
    end.



%% list_auth/2

list_auth([], _Addr, _Port, _Dir, Acc) ->
    Acc;
list_auth([{_Name, {ETS, DETS}}|Tables], Addr, Port, Dir, Acc) ->
    case ets:match_object(ETS, {success, {{'_', Dir, Addr, Port}, '_'}}) of
	[] ->
	    list_auth(Tables, Addr, Port, Dir, Acc);
	List ->
	    TN = universal_time(),
	    NewAcc = lists:foldr(fun({success,{{U,Ad,P,D},T}},Ac) -> 
					 if
					     T-TN > 0 ->
						 [U|Ac];
					     true ->
						 Rec = {success,
							{{U,Ad,P,D},T}},
						 ets:match_delete(ETS,Rec),
						 dets:match_delete(DETS,Rec),
						 Ac
					 end
				 end,
				 Acc, List),
	    list_auth(Tables, Addr, Port, Dir, NewAcc)
    end.


%% list_blocked/2

list_blocked([], _Addr, _Port, _Dir, Acc) ->
    ?hdrv("list blocked", [{acc, Acc}]), 
    TN = universal_time(),
    lists:foldl(fun({U,Ad,P,D,T}, Ac) ->
			if
			    T-TN > 0 ->
				[{U,Ad,P,D,local_time(T)}|Ac];
			    true ->
				Ac
			end
		end, 
		[], Acc);
list_blocked([{_Name, {ETS, _DETS}}|Tables], Addr, Port, Dir, Acc) ->
    ?hdrv("list blocked", [{ets, ETS}, {tab2list, ets:tab2list(ETS)}]), 
    List = ets:match_object(ETS, {blocked_user, 
				  {'_',Addr,Port,Dir,'_'}}),
    
    NewBlocked = lists:foldl(fun({blocked_user, X}, A) -> 
				     [X|A] end, Acc, List),
    
    list_blocked(Tables, Addr, Port, Dir, NewBlocked).


%%
%% sync_dets_to_ets/2
%%
%% Reads dets-table DETS and syncronizes it with the ets-table ETS.
%%
sync_dets_to_ets(DETS, ETS) ->
    dets:traverse(DETS, fun(X) ->
				ets:insert(ETS, X),
				continue
			end).

%%
%% check_blocked_user/7 -> true | false
%%
%% Check if a specific user is blocked from access.
%%
%% The sideeffect of this routine is that it unblocks also other users
%% whos blocking time has expired. This to keep the tables as small
%% as possible.
%%
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule) ->
    TN = universal_time(),
    BlockList = ets:match_object(ETS, {blocked_user, {User, '_', '_', '_', '_'}}), 
    Blocked = lists:foldl(fun({blocked_user, X}, A) ->
				  [X|A] end, [], BlockList),
    check_blocked_user(Info,User,Dir,
		       Addr,Port,ETS,DETS,TN,Blocked,CBModule).

check_blocked_user(_Info, _User, _Dir, _Addr, _Port, _ETS, _DETS, _TN,
		   [], _CBModule) ->
    false;
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, 
		   [{User,Addr,Port,Dir,T}| _], CBModule) ->
    TD = T-TN,
    if
	TD =< 0 ->
	    %% Blocking has expired, remove and grant access.
	    unblock_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule),
	    false;
	true ->
	    true
    end;
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, 
		   [{OUser,ODir,OAddr,OPort,T}|Ls], CBModule) ->
    TD = T-TN,
    if
	TD =< 0 ->
	    %% Blocking has expired, remove.
	    unblock_user(Info, OUser, ODir, OAddr, OPort, 
			 ETS, DETS, CBModule);
	true ->
	    true
    end,
    check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, 
		       TN, Ls, CBModule).

unblock_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule) ->
    Reason =
	io_lib:format("User ~s was removed from the block list for dir ~s",
			 [User, Dir]),
    mod_log:security_log(Info, lists:flatten(Reason)),
    user_unblock_event(CBModule,Addr,Port,Dir,User),
    dets:match_delete(DETS, {blocked_user, {User, Addr, Port, Dir, '_'}}),
    ets:match_delete(ETS, {blocked_user, {User, Addr, Port, Dir, '_'}}).
  

make_name(Addr,Port) ->
    httpd_util:make_name("httpd_security",Addr,Port).

make_name(Addr,Port,Num) ->
    httpd_util:make_name("httpd_security",Addr,Port,
			 "__" ++ integer_to_list(Num)).


auth_fail_event(Mod,Addr,Port,Dir,User,Passwd) ->
    event(auth_fail,Mod,Addr,Port,Dir,[{user,User},{password,Passwd}]).

user_block_event(Mod,Addr,Port,Dir,User) ->
    event(user_block,Mod,Addr,Port,Dir,[{user,User}]).

user_unblock_event(Mod,Addr,Port,Dir,User) ->
    event(user_unblock,Mod,Addr,Port,Dir,[{user,User}]).

event(Event, Mod, undefined, Port, Dir, Info) ->
    ?hdrt("event", 
	  [{event, Event}, {mod, Mod}, {port, Port}, {dir, Dir}]),
    (catch Mod:event(Event,Port,Dir,Info));
event(Event, Mod, any, Port, Dir, Info) ->
    ?hdrt("event", 
	  [{event, Event}, {mod, Mod}, {port, Port}, {dir, Dir}]),
    (catch Mod:event(Event,Port,Dir,Info));
event(Event, Mod, Addr, Port, Dir, Info) ->
    ?hdrt("event", 
	  [{event, Event}, {mod, Mod}, 
	   {addr, Addr}, {port, Port}, {dir, Dir}]),
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
