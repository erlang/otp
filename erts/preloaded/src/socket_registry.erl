%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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

%% =========================================================================
%%
%% This is a registry process for the socket module.
%% The nif sends info here about all created and deleted socket(s).
%% It is also used for misc work related to sockets, such as
%% monitor/demonitor of sockets.
%%
%% =========================================================================

-module(socket_registry).

-compile({no_auto_import, [monitor/2]}).

-export([
         start/0,
         number_of/0,
         which_sockets/1,
	 number_of_monitors/0, number_of_monitors/1,
         which_monitors/1,
	 monitor/1, monitor/2,
	 demonitor/1
        ]).


%% Cross-ref information needed for each monitor
-record(xref,
	{
	  %% The monitor (ID)
	  mref  :: reference(),

	  %% The actual socket being monitored
	  sock  :: socket:socket(),

	  %% The "socket" to be part of the DOWN message
	  msock :: term(),

	  %% The process doing the monitoring
	  pid   :: pid()
	 }).

%% Each process that monitor a socket:
-record(user,
	{
	  pid       :: pid(),        %% The process doing the monitoring
	  mref      :: reference(),  %% Our monitor to this process
	  %% We actually only need this to know when we
	  %% no longer need to monitor the process!
	  mons = [] :: [reference()] %% All (socket-) monitors of this process
	 }).

%% Info about each (known) socket
-record(info,
        {
	  sock       :: socket:socket(),
	  %% Add time - erlang:monotonic_time(milli_seconds)
	  atime      :: integer(),
	  mons  = [] :: [reference()]
	 }).

-record(state,
	{
	  %% The socket "database"
	  db   = [] :: [info()],
	  
	  %% CrossRef: monitor -> socket
	  %% [demonitor]
	  xref = [] :: [xref()],

	  %% Processes that has monitors
	  user = [] :: [user()]
	}).

-type xref()    :: #xref{}.
-type user()    :: #user{}.
-type info()    :: #info{}.

-define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).


%% =========================================================================

%% This is not a "normal" start function. Instead its the entry
%% function for the socket registry process.
start() ->
    erlang:register(?MODULE, self()),
    process_flag(trap_exit, true),
    loop(#state{}).

number_of() ->
    request(number_of).

which_sockets(Filter)
  when is_boolean(Filter);
       tuple_size(Filter) =:= 2;
       is_function(Filter, 1) ->
    case request({which_sockets, Filter}) of
        badarg ->
            erlang:error(badarg, [Filter]);
        Sockets ->
            Sockets
    end;
which_sockets(Filter) ->
    erlang:error(badarg, [Filter]).


number_of_monitors() ->
    request(number_of_monitors).

number_of_monitors(Pid) when is_pid(Pid) ->
    request({number_of_monitors, Pid}).

which_monitors(Pid) when is_pid(Pid) ->
    request({which_monitors, Pid}).


monitor(Socket) ->
    monitor(Socket, #{}).

monitor(Socket, Opts) when is_map(Opts) ->
    case request({monitor, Socket, Opts}) of
	{ok, MRef} ->
	    MRef;
	{error, _Reason} = ERROR ->
	    ERROR
    end;
monitor(Socket, Opts) ->
    erlang:error(badarg, [Socket, Opts]).

demonitor(MRef) when is_reference(MRef) ->
    %% ?DBG(MRef),
    case request({demonitor, MRef}) of
	ok ->
	    ok;
	{error, _Reason} = ERROR ->
	    ERROR
    end;
demonitor(MRef) ->
    erlang:error(badarg, [MRef]).


%% =========================================================================

loop(State) ->
    receive
        {'$socket', add, Socket} ->
	    %% ?DBG([add, Socket]),
            loop( handle_add_socket(State, Socket) );

        {'$socket', del, Socket} ->
	    %% ?DBG([del, Socket]),
            loop( handle_delete_socket(State, Socket) );

        {'$socket', sendfile_deferred_close = Fname, Socket} ->
            Closer =
                fun () ->
                        try prim_socket:Fname(Socket) of
                            ok ->
                                ok;
                            Other ->
                                log_msg(
                                  warning,
                                  "socket-registry "
                                  "sendfile_deferred_close:"
                                  "~n   [~p] ~p",
                                  [Socket, Other])
                        catch Class : Reason : Stacktrace->
                                log_msg(
                                  warning,
                                  "socket-registry "
                                  "sendfile_deferred_close:"
                                  "~n   [~p] (~p:~p)"
                                  "~n      ~p",
                                  [Socket, Class, Reason, Stacktrace])
                        end
                end,
            _ = spawn(Closer),
            loop(State);

        {?MODULE, request, From, ReqId, Req} = _REQ ->
	    %% ?DBG([request, From, ReqId, Req]),
            {NewState, Reply} = handle_request(State, Req, From),
            reply(ReqId, From, Reply),
            loop( NewState );

	{'DOWN', _MRef, process, Pid, _Info} ->
	    %% ?DBG([down, _MRef, Pid, _Info]),
	    NewState = handle_user_down(State, Pid),
	    loop( NewState );
        
        Msg ->
	    %% ?DBG(Msg),
            NewState = handle_unexpected_msg(State, Msg),
            loop(NewState)
    end.


%% =========================================================================


%% =========================================================================

handle_add_socket(#state{db = DB} = State, Sock) ->
    DB2 = [#info{sock = Sock, atime = timestamp()} | DB],
    State#state{db = DB2}.

handle_delete_socket(#state{db   = DB,
			    xref = XRef,
			    user = Users} = State, Sock) ->
    case db_sock_lookup(DB, Sock) of
	{value, #info{mons = Mons}} ->
	    handle_send_down(Mons, XRef),
	    Users2 = user_sock_delete_update(Mons, Users, XRef),
	    XRef2  = xref_sock_delete(XRef, Sock),
	    DB2    = db_sock_delete(DB, Sock),
	    State#state{db = DB2, xref = XRef2, user = Users2};
	false ->
	    State
    end.

user_sock_delete_update([], Users, _XRef) ->
    Users;
user_sock_delete_update([Mon|Mons], Users, XRef) ->
    %% ?DBG(['try find monitor', Mon]),
    case xref_mref_lookup(XRef, Mon) of
	{value, #xref{pid = Pid}} ->
	    %% ?DBG(['found xref - with user', Pid]),
	    case user_pid_lookup(Users, Pid) of
		{value, #user{mref = MRef, mons = [Mon]}} ->
		    %% ?DBG(['only one monitor -> delete user', MRef]),
		    erlang:demonitor(MRef, [flush]),
		    Users2 = user_pid_delete(Users, Pid),
		    user_sock_delete_update(Mons, Users2, XRef);
		{value, #user{mons = UMons} = User} ->
		    %% ?DBG(['several monitor(s)']),
		    UMons2 = lists:delete(Mon, UMons),
		    User2  = User#user{mons = UMons2},
		    Users2 = lists:keyreplace(Pid, #user.pid,
					      Users, User2),
		    user_sock_delete_update(Mons, Users2, XRef);
		false -> % race?
		    %% ?DBG(['no user found']),
		    user_sock_delete_update(Mons, Users, XRef)
	    end;
	false -> % race?
	    %% ?DBG(['no xref found']),
	    user_sock_delete_update(Mons, Users, XRef)
    end.

handle_send_down([], _XRef) ->
    ok;
handle_send_down([MRef|Mons], XRef) ->
    case xref_mref_lookup(XRef, MRef) of
	{value, #xref{pid = Pid, msock = MSock}} ->
	    send_down(Pid, MRef, MSock),
	    handle_send_down(Mons, XRef);
	false -> % race?
	    handle_send_down(Mons, XRef)
    end.

send_down(Pid, MRef, MSock) ->
    %% We do not yet have a 'reason', so use 'closed'...
    Pid ! mk_down_msg(MRef, MSock, closed).



handle_request(#state{db = DB} = State, number_of, _From) ->
    {State, db_size(DB)};

handle_request(#state{xref = XRef} = State, number_of_monitors, _From) ->
    {State, xref_size(XRef)};

handle_request(#state{user = Users} = State,
	       {number_of_monitors, Pid}, _From) ->
    {State, user_size(Users, Pid)};

handle_request(#state{db = DB} = State, {which_sockets, Filter}, _From) ->
    {State, do_which_sockets(DB, Filter)};

handle_request(#state{user = Users} = State, {which_monitors, Pid}, _From) ->
    {State, user_which_monitors(Users, Pid)};

handle_request(State, {monitor, Socket, Opts}, From) ->
    do_monitor_socket(State, Socket, Opts, From);

handle_request(State, {demonitor, MRef}, From) ->
    %% ?DBG({demonitor, MRef, From}),
    do_demonitor_socket(State, MRef, From);

handle_request(State, BadRequest, _From) ->
    {State, {error, {bad_request, BadRequest}}}.


%% ---

handle_user_down(#state{db = DB, xref = XRefs, user = Users} = State, Pid) ->
    case user_pid_lookup(Users, Pid) of
	{value, _} ->
	    %% Get info about all sockets monitored by this process
	    SMRefs = [SMRef ||
			#xref{mref = SMRef,
				    pid  = MPid} <- XRefs, (MPid =:= Pid)],
	    {DB2, XRefs2} = handle_user_down_cleanup(SMRefs, DB, XRefs),
	    Users2 = user_pid_delete(Users, Pid),
	    State#state{db = DB2, xref = XRefs2, user = Users2};
	false -> % race
	    State
    end.

handle_user_down_cleanup([], DB, XRefs) ->
    {DB, XRefs};
handle_user_down_cleanup([SMRef|SMRefs], DB, XRefs) ->
    case xref_mref_lookup(XRefs, SMRef) of
	{value, #xref{sock = Sock}} ->
	    case db_sock_lookup(DB, Sock) of
		{value, #info{mons = Mons} = SI} ->
		    Mons2  = lists:delete(SMRef, Mons),
		    SI2    = SI#info{mons = Mons2},
		    DB2    = db_sock_replace(DB, Sock, SI2),
		    XRefs2 = xref_mref_delete(XRefs, SMRef),
		    handle_user_down_cleanup(SMRefs, DB2, XRefs2);
		false -> % race?
		    %% This should be possible, but handle it anyway
		    XRefs2 = xref_mref_delete(XRefs, SMRef),
		    handle_user_down_cleanup(SMRefs, DB, XRefs2)
	    end;
	false -> % race?
	    handle_user_down_cleanup(SMRefs, DB, XRefs)
    end.


%% ---

handle_unexpected_msg(DB, {'EXIT', Pid, Reason}) ->
    F = "socket-registry received unexpected exit from ~p:"
        "~n   ~p",
    A = [Pid, Reason],
    log_msg(warning, F, A),
    DB;
handle_unexpected_msg(DB, X) ->
    F = "socket-registry received unexpected:"
        "~n   ~p",
    A = [X],
    log_msg(warning, F, A),
    DB.

%% This is "stolen" from the init process. But level is set to warning instead.
%% This "may" not be what you want, but we can always decrease to info instead...
%% Also, we may receive (unexpected) messages that should be classified
%% differently (info, warning, ...)...
%%
%% This is equal to calling logger:[info|warning]/3 which we don't
%% want to do from this process, at least not during
%% system boot. We don't want to call logger:timestamp()
%% either.
%%

log_msg(Level, F, A) ->
    log_msg( mk_log_msg(Level, F, A) ).
%%
log_msg(Msg) ->
    _ = catch logger ! Msg,
    ok.


%% mk_log_msg(info, F, A) ->
%%     mk_log_msg(info, info_msg, F, A);
mk_log_msg(warning, F, A) ->
    mk_log_msg(warning, warning_msg, F, A).

mk_log_msg(Level, Tag, F, A) ->
    Meta = #{pid          => self(),
             gl           => erlang:group_leader(),
             time         => os:system_time(microsecond),
             error_logger => #{tag => Tag}},
    {log, Level, F, A, Meta}.

mk_down_msg(MRef, MSock, Info) ->
    {'DOWN', MRef, socket, MSock, Info}.


%% ---

db_size(DB) ->
    length(DB).

do_which_sockets(DB, Filter) ->
    try
        [Sock ||
            #info{sock = Sock} <- DB,
            if
                is_boolean(Filter) ->
                    Filter;
                true ->
                    SockInfo = socket:info(Sock),
                    case Filter of
                        {Field, Value} ->
                            case SockInfo of
                                #{Field := Value} ->
                                    true;
                                #{} ->
                                    false
                            end;
                        _ when is_function(Filter, 1) ->
                            Filter(SockInfo) =:= true
                    end
            end]
    catch _ : _ ->
            %% Filter/1 must have failed
            badarg
    end.
    
do_monitor_socket(#state{db   = DB,
			 xref = XRefs,
			 user = Users} = State, Sock, Opts, From) ->
    case db_sock_lookup(DB, Sock) of
	{value, #info{mons = Mons} = SI} ->
	    %% Check if this process has already monitored this socket
	    %% Or shall we allow multiple monitors?
	    %% That is, the same process can have multiple monitors
	    %% to the same socket?
	    case xref_pid_lookup(Mons, From) of
		{value, #xref{mref = MRef}} ->
		    Result = {ok, MRef},
		    {State, Result};
		false ->
		    %% We need to keep track of the process
		    %% We also need to know when we no longer need to
		    %% monitor the process...
		    SMRef  = make_ref(),
		    Mons2  = [SMRef|Mons],
		    SI2    = SI#info{mons = Mons2},
		    DB2    = db_sock_replace(DB, Sock, SI2),
		    MSock  = maps:get(msocket, Opts, Sock),
		    XRef   = #xref{mref  = SMRef,
				   sock  = Sock,
				   msock = MSock,
				   pid   = From},
		    XRefs2 = [XRef|XRefs],
		    Users2 = user_pid_add(Users, From, SMRef),
		    Result = {ok, SMRef},
		    {State#state{db = DB2, xref = XRefs2, user = Users2},
		     Result}
	    end;
	false ->
	    Result = {error, unknown_socket},
	    {State, Result}
    end.


do_demonitor_socket(#state{db = DB, xref = XRefs, user = Users} = State,
		    MRef, From) ->
    case xref_mref_lookup(XRefs, MRef) of
	{value, #xref{sock = Sock, pid = From}} ->
	    %% ?DBG({xref, Sock, Pid}),
	    %% So far so good
	    case db_sock_lookup(DB, Sock) of
		%% So faar so goood
		{value, #info{mons = Mons} = SI} ->
		    %% ?DBG({mons, Mons}),
		    Mons2  = lists:delete(MRef, Mons),
		    Users2 = user_pid_delete_mon(Users, From, MRef),
		    SI2    = SI#info{mons = Mons2},
		    DB2    = db_sock_replace(DB, Sock, SI2),
		    XRefs2 = xref_mref_delete(XRefs, MRef),
		    Result = ok,
		    {State#state{db = DB2, xref = XRefs2, user = Users2},
		     Result};
		false ->
		    %% The socket has been deleted => race
		    Result = {error, unknown_socket},
		    XRefs2 = xref_mref_delete(XRefs, MRef),
		    {State#state{xref = XRefs2}, Result}
	    end;
	{value, #xref{sock = _Sock, pid = _Pid}} ->
	    %% Not the owner of this monitor
	    Result = {error, not_owner},
	    {State, Result};
	false ->
	    %% No such monitor => race
	    Result = {error, unknown_monitor},
	    {State, Result}
    end.



%% =========================================================================

%% --- XRef utility functions ---

xref_sock_delete(XRefs, Sock) when is_list(XRefs) ->
    xref_delete(XRefs, #xref.sock, Sock).

xref_mref_delete(XRefs, MRef) when is_list(XRefs) andalso is_reference(MRef) ->
    xref_delete(XRefs, #xref.mref, MRef).

xref_delete([] = XRefs, _Pos, _Key) ->
    XRefs;
xref_delete(XRefs, Pos, Key) ->
    case lists:keydelete(Key, Pos, XRefs) of
	XRefs ->
	    XRefs;
	XRefs2 ->
	    xref_delete(XRefs2, Pos, Key)
    end.


xref_mref_lookup(XRefs, MRef) when is_list(XRefs) andalso is_reference(MRef) ->
    xref_lookup(XRefs, #xref.mref, MRef).

xref_pid_lookup(XRefs, Pid) when is_list(XRefs) andalso is_pid(Pid) ->
    xref_lookup(XRefs, #xref.pid, Pid).

xref_lookup(XRefs, Pos, Key) ->
    lists:keysearch(Key, Pos, XRefs).

xref_size(XRefs) when is_list(XRefs) ->
    length(XRefs).


%% --- DB utility functions ---

db_sock_lookup(DB, Sock) when is_list(DB) ->
    db_lookup(DB, #info.sock, Sock).

db_lookup(DB, Pos, Key) ->
    lists:keysearch(Key, Pos, DB).


db_sock_delete(DB, Sock) when is_list(DB) ->
    db_delete(DB, #info.sock, Sock).

db_delete(DB, Pos, Key) ->
    lists:keydelete(Key, Pos, DB).


db_sock_replace(DB, Sock, Info) when is_list(DB) ->
    db_replace(DB, #info.sock, Sock, Info).

db_replace(DB, Pos, Key, Info) ->
    lists:keyreplace(Key, Pos, DB, Info).


%% --- Users utility functions ---

%% Side effect: We will monitor the process (if this was the first monitor)
user_pid_add(Users, Pid, SMRef)
  when is_list(Users) andalso is_pid(Pid) andalso is_reference(SMRef) ->
    Pos = #user.pid,
    case lists:keysearch(Pid, Pos, Users) of
	{value, #user{mons = Mons} = User} ->
	    Mons2 = [SMRef|Mons],
	    User2 = User#user{mons = Mons2},
	    lists:keyreplace(Pid, Pos, Users, User2);
	false ->
	    MRef = erlang:monitor(process, Pid),
	    User = #user{pid = Pid, mref = MRef, mons = [SMRef]},
	    [User|Users]
    end.

%% Side effect: We will demonitor the process (if this was the last monitors)
user_pid_delete_mon(Users, Pid, SMRef)
  when is_list(Users) andalso is_reference(SMRef) ->
    Pos = #user.pid,
    case lists:keysearch(Pid, Pos, Users) of

	%% The last monitor of this process
	{value, #user{mref = MRef, mons = [SMRef]}} ->
	    erlang:demonitor(MRef, [flush]),
	    lists:keydelete(Pid, Pos, Users);

	%% At least one more
	{value, #user{mons = Mons} = User} ->
	    Mons2 = lists:delete(SMRef, Mons),
	    User2 = User#user{mons = Mons2},
	    lists:keyreplace(Pid, Pos, Users, User2);

	% race?
	false ->
	    Users
    end.

user_pid_delete(Users, Pid) when is_list(Users) ->
    lists:keydelete(Pid, #user.pid, Users).

user_pid_lookup(Users, Pid) when is_list(Users) ->
    lists:keysearch(Pid, #user.pid, Users).

user_which_monitors(Users, Pid) when is_list(Users) andalso is_pid(Pid) ->
    case user_pid_lookup(Users, Pid) of
	{value, #user{mons = Mons}} ->
	    Mons;
	false ->
	    []
    end.

user_size(Users, Pid) when is_list(Users) andalso is_pid(Pid) ->
    length(user_which_monitors(Users, Pid)).


%% =========================================================================

request(Req) ->
    ReqId  = make_ref(),
    From = self(),
    ReqMsg = {?MODULE, request, From, ReqId, Req},
    Registry = whoami(),    
    erlang:send(Registry, ReqMsg),
    receive
        {?MODULE, reply, Registry, ReqId, Reply} ->
            Reply
    end.

reply(ReqId, From, Reply) ->
    Registry = self(),
    RepMsg = {?MODULE, reply, Registry, ReqId, Reply},
    erlang:send(From, RepMsg).


%% =========================================================================

whoami() ->
    whereis(?MODULE).

timestamp() ->
    erlang:monotonic_time(milli_seconds).
