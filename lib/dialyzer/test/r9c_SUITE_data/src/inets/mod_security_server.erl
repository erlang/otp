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
%%     $Id: mod_security_server.erl,v 1.1 2008/12/17 09:53:36 mikpe Exp $
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
-include("httpd_verbosity.hrl").


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
-export([start_link/3,
	 init/1,
	 handle_info/2, handle_call/3, handle_cast/2,
	 terminate/2,
	 code_change/3]).

-export([verbosity/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% External API                                                     %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% start_link/3
%%
%% NOTE: This is called by httpd_misc_sup when the process is started
%%

start_link(Addr, Port, Verbosity) ->
    ?vtrace("start_link -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p", [Addr, Port]),
    Name = make_name(Addr, Port),
    gen_server:start_link({local, Name}, ?MODULE, [Verbosity],
			  [{timeout, infinity}]).


%% start/2
%% Called  by the mod_security module.

start(Addr, Port) ->
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
	    Verbosity = get(security_verbosity),
	    case httpd_misc_sup:start_sec_server(Addr, Port, Verbosity) of
		{ok, Pid} ->
		    put(security_server, Pid),
		    ok;
		Error ->
		    exit({failed_start_security_server, Error})
	    end;
	_ -> %% Already started...
	    ok
    end.


%% stop

stop(Port) ->
    stop(undefined, Port).
stop(Addr, Port) ->
    Name = make_name(Addr, Port),
    case whereis(Name) of
	undefined ->
	    ok;
	_ ->
	    httpd_misc_sup:stop_sec_server(Addr, Port)
    end.


%% verbosity

verbosity(Addr, Port, Verbosity) ->
    Name = make_name(Addr, Port),
    Req  = {verbosity, Verbosity},
    call(Name, Req).


%% list_blocked_users

list_blocked_users(Addr, Port) ->
    Name = make_name(Addr,Port),
    Req  = {list_blocked_users, Addr, Port, '_'},
    call(Name, Req).

list_blocked_users(Addr, Port, Dir) ->
    Name = make_name(Addr, Port),
    Req  = {list_blocked_users, Addr, Port, Dir},
    call(Name, Req).


%% block_user

block_user(User, Addr, Port, Dir, Time) ->
    Name = make_name(Addr, Port),
    Req  = {block_user, User, Addr, Port, Dir, Time},
    call(Name, Req).


%% unblock_user

unblock_user(User, Addr, Port) ->
    Name = make_name(Addr, Port),
    Req  = {unblock_user, User, Addr, Port, '_'},
    call(Name, Req).

unblock_user(User, Addr, Port, Dir) ->
    Name = make_name(Addr, Port),
    Req  = {unblock_user, User, Addr, Port, Dir},
    call(Name, Req).


%% list_auth_users

list_auth_users(Addr, Port) ->
    Name = make_name(Addr, Port),
    Req  = {list_auth_users, Addr, Port, '_'},
    call(Name, Req).

list_auth_users(Addr, Port, Dir) ->
    Name = make_name(Addr,Port),
    Req  = {list_auth_users, Addr, Port, Dir},
    call(Name, Req).


%% new_table

new_table(Addr, Port, TabName) ->
    Name = make_name(Addr,Port),
    Req  = {new_table, Addr, Port, TabName},
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

%% init

init([undefined]) ->
    init([?default_verbosity]);
init([Verbosity]) ->
    ?DEBUG("init -> entry with Verbosity: ~p",[Verbosity]),
    process_flag(trap_exit, true),
    put(sname, sec),
    put(verbosity, Verbosity),
    ?vlog("starting",[]),
    {ok, []}.


%% handle_call

handle_call(stop, _From, Tables) ->
    ?vlog("stop",[]),
    {stop, normal, ok, []};


handle_call({verbosity,Verbosity}, _From, Tables) ->
    ?vlog("set verbosity to ~p",[Verbosity]),
    OldVerbosity = get(verbosity),
    put(verbosity,Verbosity),
    ?vdebug("old verbosity: ~p",[OldVerbosity]),
    {reply,OldVerbosity,Tables};


handle_call({block_user, User, Addr, Port, Dir, Time}, _From, Tables) ->
    ?vlog("block user '~p' for ~p",[User,Dir]),
    Ret = block_user_int({User, Addr, Port, Dir, Time}),
    ?vdebug("block user result: ~p",[Ret]),
    {reply, Ret, Tables};


handle_call({list_blocked_users, Addr, Port, Dir}, _From, Tables) ->
    ?vlog("list blocked users for ~p",[Dir]),
    Blocked = list_blocked(Tables, Addr, Port, Dir, []),
    ?vdebug("list blocked users: ~p",[Blocked]),
    {reply, Blocked, Tables};


handle_call({unblock_user, User, Addr, Port, Dir}, _From, Tables) ->
    ?vlog("unblock user '~p' for ~p",[User,Dir]),
    Ret = unblock_user_int({User, Addr, Port, Dir}),
    ?vdebug("unblock user result: ~p",[Ret]),
    {reply, Ret, Tables};


handle_call({list_auth_users, Addr, Port, Dir}, _From, Tables) ->
    ?vlog("list auth users for ~p",[Dir]),
    Auth = list_auth(Tables, Addr, Port, Dir, []),
    ?vdebug("list auth users result: ~p",[Auth]),
    {reply, Auth, Tables};


handle_call({new_table, Addr, Port, Name}, _From, Tables) ->
    case lists:keysearch(Name, 1, Tables) of
	{value, {Name, {Ets, Dets}}} ->
	    ?DEBUG("handle_call(new_table) -> we already have this table: ~p",
		   [Name]),
	    ?vdebug("new table; we already have this one: ~p",[Name]),
	    {reply, {ok, {Ets, Dets}}, Tables};
	false ->
	    ?LOG("handle_call(new_table) -> new_table: Name = ~p",[Name]),
	    ?vlog("new table: ~p",[Name]),
	    TName = make_name(Addr,Port,length(Tables)),
	    ?DEBUG("handle_call(new_table) -> TName: ~p",[TName]),
	    ?vdebug("new table: ~p",[TName]),
	    case dets:open_file(TName, [{type, bag}, {file, Name},
					{repair, true},
					{access, read_write}]) of
		{ok, DFile} ->
		    ETS = ets:new(TName, [bag, private]),
		    sync_dets_to_ets(DFile, ETS),
		    NewTables = [{Name, {ETS, DFile}}|Tables],
		    ?DEBUG("handle_call(new_table) -> ~n"
			   "       NewTables: ~p",[NewTables]),
		    ?vtrace("new tables: ~p",[NewTables]),
		    {reply, {ok, {ETS, DFile}}, NewTables};
		{error, Err} ->
		    ?LOG("handle_call -> Err: ~p",[Err]),
		    ?vinfo("failed open dets file: ~p",[Err]),
		    {reply, {error, {create_dets, Err}}, Tables}
	    end
    end;

handle_call(delete_tables, _From, Tables) ->
    ?vlog("delete tables",[]),
    lists:foreach(fun({Name, {ETS, DETS}}) ->
			  dets:close(DETS),
			  ets:delete(ETS)
		  end, Tables),
    {reply, ok, []};

handle_call({check_blocked_user, [Info, User, SDirData]}, _From, Tables) ->
    ?vlog("check blocked user '~p'",[User]),
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    Dir = httpd_util:key1search(SDirData, path),
    Addr = httpd_util:key1search(SDirData, bind_address),
    Port = httpd_util:key1search(SDirData, port),
    CBModule = httpd_util:key1search(SDirData, callback_module, no_module_at_all),
    ?vdebug("call back module: ~p",[CBModule]),
    Ret = check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule),
    ?vdebug("check result: ~p",[Ret]),
    {reply, Ret, Tables};
handle_call(Request,From,Tables) ->
    ?vinfo("~n   unknown call '~p' from ~p",[Request,From]),
    {reply,ok,Tables}.


%% handle_cast

handle_cast({store_failed_auth, [Info, DecodedString, SDirData]}, Tables) ->
    ?vlog("store failed auth",[]),
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    Dir  = httpd_util:key1search(SDirData, path),
    Addr = httpd_util:key1search(SDirData, bind_address),
    Port = httpd_util:key1search(SDirData, port),
    {ok, [User,Password]} = httpd_util:split(DecodedString,":",2),
    ?vdebug("user '~p' and password '~p'",[User,Password]),
    Seconds = universal_time(),
    Key = {User, Dir, Addr, Port},

    %% Event
    CBModule = httpd_util:key1search(SDirData, callback_module, no_module_at_all),
    ?vtrace("call back module: ~p",[CBModule]),
    auth_fail_event(CBModule,Addr,Port,Dir,User,Password),

    %% Find out if any of this user's other failed logins are too old to keep..
    ?vtrace("remove old login failures",[]),
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	[] ->
	    ?vtrace("no old login failures",[]),
	    no;
	List when list(List) ->
	    ?vtrace("~p old login failures",[length(List)]),
	    ExpireTime = httpd_util:key1search(SDirData, fail_expire_time, 30)*60,
	    ?vtrace("expire time ~p",[ExpireTime]),
	    lists:map(fun({failed, {TheKey, LS, Gen}}) ->
			      Diff = Seconds-LS,
			      if
				  Diff > ExpireTime ->
				      ?vtrace("~n   '~p' is to old to keep: ~p",
					      [TheKey,Gen]),
				      ets:match_delete(ETS, {failed, {TheKey, LS, Gen}}),
				      dets:match_delete(DETS, {failed, {TheKey, LS, Gen}});
				  true ->
				      ?vtrace("~n   '~p' is not old enough: ~p",
					      [TheKey,Gen]),
				      ok
			      end
		      end,
		      List);
	O ->
	    ?vlog("~n   unknown login failure search resuylt: ~p",[O]),
	    no
    end,

    %% Insert the new failure..
    Generation = length(ets:match_object(ETS, {failed, {Key, '_', '_'}})),
    ?vtrace("insert ('~p') new login failure: ~p",[Key,Generation]),
    ets:insert(ETS, {failed, {Key, Seconds, Generation}}),
    dets:insert(DETS, {failed, {Key, Seconds, Generation}}),

    %% See if we should block this user..
    MaxRetries = httpd_util:key1search(SDirData, max_retries, 3),
    BlockTime = httpd_util:key1search(SDirData, block_time, 60),
    ?vtrace("~n   Max retries ~p, block time ~p",[MaxRetries,BlockTime]),
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	List1 ->
	    ?vtrace("~n   ~p tries so far",[length(List1)]),
	    if
		length(List1) >= MaxRetries ->
		    %% Block this user until Future
		    ?vtrace("block user '~p'",[User]),
		    Future = Seconds+BlockTime*60,
		    ?vtrace("future: ~p",[Future]),
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
		true ->
		    ?vtrace("still some tries to go",[]),
		    no
	    end;
	Other ->
	    no
    end,
    {noreply, Tables};

handle_cast({store_successful_auth, [User, Addr, Port, SDirData]}, Tables) ->
    ?vlog("store successfull auth",[]),
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    AuthTimeOut = httpd_util:key1search(SDirData, auth_timeout, 30),
    Dir = httpd_util:key1search(SDirData, path),
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
    ?vinfo("~n   unknown cast '~p'",[Req]),
    error_msg("security server got unknown cast: ~p",[Req]),
    {noreply, Tables}.


%% handle_info

handle_info(Info, State) ->
    ?vinfo("~n   unknown info '~p'",[Info]),
    {noreply, State}.


%% terminate

terminate(Reason, _Tables) ->
    ?vlog("~n   Terminating for reason: ~p",[Reason]),
    ok.


%% code_change({down, ToVsn}, State, Extra)
%%
code_change({down, _}, State, _Extra) ->
    ?vlog("downgrade", []),
    {ok, State};


%% code_change(FromVsn, State, Extra)
%%
code_change(_, State, Extra) ->
    ?vlog("upgrade", []),
    {ok, State}.




%% block_user_int/2
block_user_int({User, Addr, Port, Dir, Time}) ->
    Dirs = httpd_manager:config_match(Addr, Port, {security_directory, '_'}),
    ?vtrace("block '~p' for ~p during ~p",[User,Dir,Time]),
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
	    dets:match_delete(DETS, {blocked_user, {User,Addr,Port,Dir,'_'}}),
	    ets:insert(ETS, {blocked_user, {User,Addr,Port,Dir,Future}}),
	    dets:insert(DETS, {blocked_user, {User,Addr,Port,Dir,Future}}),
	    CBModule = httpd_util:key1search(DirData, callback_module,
					     no_module_at_all),
	    ?vtrace("call back module ~p",[CBModule]),
	    user_block_event(CBModule,Addr,Port,Dir,User),
	    true;
	_ ->
	    {error, no_such_directory}
    end.


find_dirdata([], _Dir) ->
    false;
find_dirdata([{security_directory, DirData}|SDirs], Dir) ->
    case lists:keysearch(path, 1, DirData) of
	{value, {path, Dir}} ->
	    {value, {data_file, {ETS, DETS}}} =
		lists:keysearch(data_file, 1, DirData),
	    {ok, DirData, {ETS, DETS}};
	_ ->
	    find_dirdata(SDirs, Dir)
    end.

%% unblock_user_int/2

unblock_user_int({User, Addr, Port, Dir}) ->
    ?vtrace("unblock user '~p' for ~p",[User,Dir]),
    Dirs = httpd_manager:config_match(Addr, Port, {security_directory, '_'}),
    ?vtrace("~n   dirs: ~p",[Dirs]),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    case ets:match_object(ETS,{blocked_user,{User,Addr,Port,Dir,'_'}}) of
		[] ->
		    ?vtrace("not blocked",[]),
		    {error, not_blocked};
		Objects ->
		    ets:match_delete(ETS, {blocked_user,
					   {User, Addr, Port, Dir, '_'}}),
		    dets:match_delete(DETS, {blocked_user,
					     {User, Addr, Port, Dir, '_'}}),
		    CBModule = httpd_util:key1search(DirData, callback_module,
						     no_module_at_all),
		    user_unblock_event(CBModule,Addr,Port,Dir,User),
		    true
	    end;
	_ ->
	    ?vlog("~n   cannot unblock: no such directory '~p'",[Dir]),
	    {error, no_such_directory}
    end.



%% list_auth/2

list_auth([], _Addr, _Port, Dir, Acc) ->
    Acc;
list_auth([{Name, {ETS, DETS}}|Tables], Addr, Port, Dir, Acc) ->
    case ets:match_object(ETS, {success, {{'_', Dir, Addr, Port}, '_'}}) of
	[] ->
	    list_auth(Tables, Addr, Port, Dir, Acc);
	List when list(List) ->
	    TN = universal_time(),
	    NewAcc = lists:foldr(fun({success,{{U,Ad,P,D},T}},Ac) ->
					 if
					     T-TN > 0 ->
						 [U|Ac];
					     true ->
						 Rec = {success,{{U,Ad,P,D},T}},
						 ets:match_delete(ETS,Rec),
						 dets:match_delete(DETS,Rec),
						 Ac
					 end
				 end,
				 Acc, List),
	    list_auth(Tables, Addr, Port, Dir, NewAcc);
	_ ->
	    list_auth(Tables, Addr, Port, Dir, Acc)
    end.


%% list_blocked/2

list_blocked([], Addr, Port, Dir, Acc) ->
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
list_blocked([{Name, {ETS, DETS}}|Tables], Addr, Port, Dir, Acc) ->
    NewBlocked =
	case ets:match_object(ETS, {blocked_user, {'_',Addr,Port,Dir,'_'}}) of
	    List when list(List) ->
		lists:foldl(fun({blocked_user, X}, A) -> [X|A] end, Acc, List);
	    _ ->
		Acc
	end,
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
    case ets:match_object(ETS, {blocked_user, {User, '_', '_', '_', '_'}}) of
	List when list(List) ->
	    Blocked = lists:foldl(fun({blocked_user, X}, A) ->
					  [X|A] end, [], List),
	    check_blocked_user(Info,User,Dir,Addr,Port,ETS,DETS,TN,Blocked,CBModule);
	_ ->
	    false
    end.
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, [], CBModule) ->
    false;
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN,
		   [{User,Addr,Port,Dir,T}|Ls], CBModule) ->
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
	    unblock_user(Info, OUser, ODir, OAddr, OPort, ETS, DETS, CBModule);
	true ->
	    true
    end,
    check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, Ls, CBModule).

unblock_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule) ->
    Reason=io_lib:format("User ~s was removed from the block list for dir ~s",
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

event(Event,Mod,undefined,Port,Dir,Info) ->
    (catch Mod:event(Event,Port,Dir,Info));
event(Event,Mod,Addr,Port,Dir,Info) ->
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
