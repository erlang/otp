%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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
%% monitor/cancel_monitor of sockets.
%%
%% =========================================================================

-module(socket_registry).
-moduledoc false.

-compile({no_auto_import, [monitor/2]}).

-export([
         start/0,
         number_of/0,
         which_sockets/1,

	 monitor/1, monitor/2,
	 cancel_monitor/1,
	 number_of_monitors/0, number_of_monitors/1,
         which_monitors/1,
	 monitored_by/1
        ]).

%% Info about each (known) socket
%% The socket database:
%%   Key:   socket:socket()
%%   Value: info()
%%
-record(info,
        {
	  %% Add time - erlang:monotonic_time(milli_seconds)
	  atime      :: integer(),
	  mons  = [] :: [reference()]
	 }).

%% Information related to (socket) monitors
%%   Key:   reference() (the socket monitor)
%%   Value: mon()
%%
-record(mon,
	{
	  %% The actual socket being monitored
	  sock  :: socket:socket(),

	  %% The "socket" to be part of the DOWN message
	  msock :: term(),

	  %% The process doing the monitoring
	  pid   :: pid()
	 }).

%% Each process that monitor a socket:
%%   Key:   pid()
%%   Value: user()
%%
-record(user,
	{
	  mref      :: reference(),  %% Our monitor to this process
	  %% We actually only need this to know when we
	  %% no longer need to monitor the process!
	  mons = [] :: [reference()] %% All (socket-) monitors of this process
	 }).

-record(state,
	{
	  %% The socket "database"
	  socks = #{},
	  
	  %% The socket monitor database
	  mons  = #{},

	  %% Each process that performs a socket monitor
	  %% needs to be monitored in turn.
	  users = #{}
	}).

%% -type mon()     :: #mon{}.
%% -type user()    :: #user{}.
%% -type info()    :: #info{}.

-define(NOT_FOUND, {?MODULE, not_found}).
-define(DBG(T), erlang:display({{self(), ?MODULE, ?LINE, ?FUNCTION_NAME}, T})).

-define(socket(SockRef), {'$socket', (SockRef)}). % As in socket.erl

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

cancel_monitor(MRef) when is_reference(MRef) ->
    %% ?DBG(MRef),
    case request({cancel_monitor, MRef}) of
	ok ->
	    ok;
	{error, _Reason} = ERROR ->
	    ERROR
    end;
cancel_monitor(MRef) ->
    erlang:error(badarg, [MRef]).


number_of_monitors() ->
    request(number_of_monitors).

number_of_monitors(Pid) when is_pid(Pid) ->
    request({number_of_monitors, Pid});
number_of_monitors(Socket) ->
    request({number_of_monitors, Socket}).

which_monitors(Pid) when is_pid(Pid) ->
    request({which_monitors, Pid});
which_monitors(Socket) ->
    request({which_monitors, Socket}).

monitored_by(Socket) ->
    request({monitored_by, Socket}).


%% =========================================================================

loop(State) ->
    receive
        {'$socket', add, Socket} ->
	    %% ?DBG([add, Socket]),
            loop( handle_add_socket(State, Socket) );

        {'$socket', del, Socket} ->
	    %% ?DBG([del, Socket]),
            loop( handle_delete_socket(State, Socket) );

        {'$socket',
         sendfile_deferred_close = Fname,
         ?socket(SockRef) = Socket} ->
            Closer =
                fun () ->
                        try prim_socket:Fname(SockRef) of
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

handle_add_socket(#state{socks = SDB} = State, Sock) ->
    SDB2 = sock_update(SDB, Sock, #info{atime = timestamp()}),
    State#state{socks = SDB2}.

handle_delete_socket(#state{socks = SDB,
			    mons  = MDB,
			    users = UDB} = State, Sock) ->
    case sock_take(SDB, Sock) of
	{#info{mons = Mons}, SDB2} ->
	    handle_send_down(Mons, MDB),
	    {UDB2, MDB2} = user_sock_delete_update(Mons, UDB, MDB),
	    State#state{socks = SDB2, mons = MDB2, users = UDB2};
	error ->
	    State
    end.

user_sock_delete_update([], UDB, MDB) ->
    {UDB, MDB};
user_sock_delete_update([Mon|Mons], UDB, MDB) ->
    %% ?DBG(['try find monitor', Mon]),
    case mon_take(MDB, Mon) of
	{#mon{pid = Pid}, MDB2} ->
	    %% ?DBG(['found xref - with user', Pid]),
	    %% Delete this user *only* if this is its last monitor
	    case user_lookup(UDB, Pid) of
		{value, #user{mref = MRef, mons = [Mon]}} ->
		    %% ?DBG(['only one monitor -> delete user', MRef]),
		    erlang:demonitor(MRef, [flush]),
		    UDB2 = user_delete(UDB, Pid),
		    user_sock_delete_update(Mons, UDB2, MDB2);
		{value, #user{mons = UMons} = User} ->
		    %% ?DBG(['several monitor(s)']),
		    UMons2 = lists:delete(Mon, UMons),
		    User2  = User#user{mons = UMons2},
		    UDB2   = user_update(UDB, Pid, User2),
		    user_sock_delete_update(Mons, UDB2, MDB2);
		false -> % race?
		    %% ?DBG(['no user found']),
		    user_sock_delete_update(Mons, UDB, MDB2)
	    end;
	error -> % race?
	    %% ?DBG(['no xref found']),
	    user_sock_delete_update(Mons, UDB, MDB)
    end.

handle_send_down([], _MDB) ->
    ok;
handle_send_down([Mon|Mons], MDB) ->
    case mon_lookup(MDB, Mon) of
	{value, #mon{pid = Pid, msock = MSock}} ->
	    send_down(Pid, Mon, MSock, closed),
	    handle_send_down(Mons, MDB);
	false -> % race?
	    handle_send_down(Mons, MDB)
    end.

send_down(Pid, Mon, MSock, Reason) ->
    %% We do not yet have a 'reason', so use 'closed'...
    Pid ! mk_down_msg(Mon, MSock, Reason),
    ok.



handle_request(#state{socks = SDB} = State, number_of, _From) ->
    {State, sock_size(SDB)};

handle_request(#state{mons = MDB} = State, number_of_monitors, _From) ->
    {State, mon_size(MDB)};

handle_request(#state{users = UDB} = State,
	       {number_of_monitors, Pid}, _From) when is_pid(Pid) ->
    {State, user_size(UDB, Pid)};
handle_request(#state{socks = SDB} = State,
	       {number_of_monitors, Socket}, _From) ->
    {State, sock_size(SDB, Socket)};

handle_request(#state{socks = SDB} = State, {which_sockets, Filter}, _From) ->
    {State, do_which_sockets(SDB, Filter)};

handle_request(#state{users = UDB} = State,
	       {which_monitors, Pid}, _From) when is_pid(Pid) ->
    {State, user_which_monitors(UDB, Pid)};
handle_request(#state{socks = SDB} = State,
	       {which_monitors, Socket}, _From) ->
    %% ?DBG({which_request, Socket}),
    {State, sock_which_monitors(SDB, Socket)};

handle_request(#state{socks = SDB, mons = MDB} = State,
	       {monitored_by, Socket}, _From) ->
    {State, handle_monitored_by(SDB, MDB, Socket)};

handle_request(State, {monitor, Socket, Opts}, From) ->
    do_monitor_socket(State, Socket, Opts, From);

handle_request(State, {cancel_monitor, MRef}, From) ->
    %% ?DBG({cancel_monitor, MRef, From}),
    do_cancel_monitor_socket(State, MRef, From);

handle_request(State, BadRequest, _From) ->
    {State, {error, {bad_request, BadRequest}}}.


%% ---

handle_user_down(#state{socks = SDB, mons = MDB, users = UDB} = State, Pid) ->
    case user_take(UDB, Pid) of
	{#user{mons = UMons}, UDB2} ->
	    %% Get info about all sockets monitored by this process
	    {SDB2, MDB2} = handle_user_down_cleanup(UMons, SDB, MDB),
	    State#state{socks = SDB2, mons = MDB2, users = UDB2};
	error -> % race
	    State
    end.

handle_user_down_cleanup([], SDB, MDB) ->
    {SDB, MDB};
handle_user_down_cleanup([Mon|Mons], SDB, MDB) ->
    case mon_take(MDB, Mon) of
	{#mon{sock = Sock}, MDB2} ->
	    case sock_lookup(SDB, Sock) of
		{value, #info{mons = SMons} = Info} ->
		    SMons2 = lists:delete(Mon, SMons),
		    Info2  = Info#info{mons = SMons2},
		    SDB2   = sock_update(SDB, Sock, Info2),
		    handle_user_down_cleanup(Mons, SDB2, MDB2);
		false -> % race?
		    handle_user_down_cleanup(Mons, SDB, MDB2)
	    end;
	error -> % race?
	    handle_user_down_cleanup(Mons, SDB, MDB)
    end.

%% ---

handle_monitored_by(SDB, MDB, Sock) ->
    case sock_lookup(SDB, Sock) of
	{value, #info{mons = Mons}} ->
	    handle_monitored_by2(Mons, MDB, []);
	false ->
	    []
    end.

handle_monitored_by2([], _MDB, Acc) ->
    lists:sort(Acc);
handle_monitored_by2([Mon|Mons], MDB, Acc) ->
    case mon_lookup(MDB, Mon) of
	{value, #mon{pid = Pid}} ->
	    case lists:member(Pid, Acc) of
		true ->
		    handle_monitored_by2(Mons, MDB, Acc);
		false ->
		    handle_monitored_by2(Mons, MDB, [Pid|Acc])
	    end;
	false ->
	    []
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
    mk_log_msg(warning, warning_msg, F, A);

mk_log_msg(error, F, A) ->
    mk_log_msg(error, error_msg, F, A).


mk_log_msg(Level, Tag, F, A) ->
    Meta = #{pid          => self(),
             gl           => erlang:group_leader(),
             time         => os:system_time(microsecond),
             error_logger => #{tag => Tag}},
    {log, Level, F, A, Meta}.


%% ---

mk_down_msg(MRef, MSock, Info) ->
    {'DOWN', MRef, socket, MSock, Info}.


%% ---

sock_size(DB) ->
    maps:size(DB).

do_which_sockets(SDB, Filter) ->
    try
        [Sock || Sock <- maps:keys(SDB),
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
    catch
	C:E:S ->
            %% Filter/1 must have failed
	    F = "socket-registry error while processing socket: "
		"~n   Class: ~p"
		"~n   Error: ~p"
		"~n   Stack: ~p",
	    A = [C, E, S],
	    log_msg(error, F, A),
            badarg
    end.
    
do_monitor_socket(#state{socks = SDB,
			 mons  = MDB,
			 users = UDB} = State, Sock, Opts, Pid) ->
    SMon  = make_ref(),
    MSock = maps:get(msocket, Opts, Sock),
    case sock_lookup(SDB, Sock) of
	{value, #info{mons = SMons} = Info} ->
	    %% Its allowed for one process to monitor the same socket
	    %% multiple times. Each call will result in a new monitor.
	    %%
	    %% We need to monitor this process, so check if this is
	    %% the first monitor created (by this process).
	    %% *We* do not create more than one monitor for each process...
	    %%
	    %% Create monitor info and update monitor sb
	    Mon  = #mon{sock  = Sock,
			msock = MSock,
			pid   = Pid},
	    MDB2 = mon_update(MDB, SMon, Mon),

	    %% Socket Info
	    SMons2 = [SMon|SMons],
	    Info2  = Info#info{mons = SMons2},
	    SDB2   = sock_update(SDB, Sock, Info2),

	    case user_lookup(UDB, Pid) of
		{value, #user{mons = UMons} = User} ->
		    %% User Info
		    UMons2 = [SMon|UMons],
		    User2  = User#user{mons = UMons2},
		    UDB2   = user_update(UDB, Pid, User2),

		    %% And we are done
		    Result = {ok, SMon},
		    {State#state{socks = SDB2, mons = MDB2, users = UDB2},
		     Result};

		false -> % First time for this process
		    %% User Info
		    MRef = erlang:monitor(process, Pid),
		    User = #user{mref = MRef, mons = [SMon]},
		    UDB2 = user_update(UDB, Pid, User),

		    %% And we are done
		    Result = {ok, SMon},
		    {State#state{socks = SDB2, mons = MDB2, users = UDB2},
		     Result}
	    end;
	false ->
	    %% We assume that this socket was "just" deleted,
	    %% and the caller has just not noticed it yet.
	    %% "Create" a monitor and trigger it (send dowm message)
	    %% directly!
	    send_down(Pid, SMon, MSock, nosock),
	    Result = {ok, SMon},
	    {State, Result}
    end.


do_cancel_monitor_socket(#state{socks = SDB, mons = MDB, users = UDB} = State,
			 Mon, Pid) ->
    case mon_lookup(MDB, Mon) of
	{value, #mon{sock = Sock, pid = Pid}} ->
	    %% So far so good
	    %% 'Pid' is the rightfull owner
	    %% ?DBG({mon, Sock, Pid}),
	    case sock_lookup(SDB, Sock) of
		%% So faar so goood
		{value, #info{mons = SMons} = Info} ->
		    %% ?DBG({smons, SMons}),
		    SMons2 = lists:delete(Mon, SMons),
		    Info2  = Info#info{mons = SMons2},
		    SDB2   = sock_update(SDB, Sock, Info2),
		    MDB2   = mon_delete(MDB, Mon),
		    case user_lookup(UDB, Pid) of
			{value, #user{mons = UMons} = User} ->
			    %% ?DBG({umons, UMons}),
			    UMons2 = lists:delete(Mon, UMons),
			    User2  = User#user{mons = UMons2},
			    UDB2   = user_update(UDB, Pid, User2),
			    Result = ok,
			    {State#state{socks = SDB2,
					 mons  = MDB2,
					 users = UDB2},
			     Result};
			false -> % race?
			    %% The socket has been deleted => race
			    Result = ok,
			    {State#state{socks = SDB2, mons = MDB2}, Result}
		    end;
		false ->
		    %% The socket has been deleted => race
		    MDB2   = mon_delete(MDB, Mon),
		    Result = {error, unknown_socket},
		    {State#state{mons = MDB2}, Result}
	    end;
	{value, #mon{sock = _Sock, pid = _Pid}} ->
	    %% *Not* the owner of this monitor
	    Result = {error, not_owner},
	    {State, Result};
	false ->
	    %% No such monitor => race
	    Result = {error, unknown_monitor},
	    {State, Result}
    end.



%% =========================================================================

%% --- Monitor database utility functions ---

mon_delete(DB, Mon) when is_map(DB) andalso is_reference(Mon) ->
    maps:remove(Mon, DB).

mon_lookup(DB, Mon) when is_map(DB) andalso is_reference(Mon) ->
    lookup(DB, Mon).

mon_take(DB, Mon) when is_map(DB) andalso is_reference(Mon) ->
    maps:take(Mon, DB).

mon_update(DB, Mon, Value) when is_map(DB) andalso is_reference(Mon) ->
    maps:put(Mon, Value, DB).

mon_size(DB) when is_map(DB) ->
    maps:size(DB).


%% --- DB utility functions ---

sock_lookup(DB, Sock) when is_map(DB) ->
    lookup(DB, Sock).

sock_take(DB, Sock) when is_map(DB) ->
    maps:take(Sock, DB).

%% sock_delete(DB, Sock) when is_map(DB) ->
%%     maps:remove(Sock, DB).

sock_update(DB, Sock, Info) when is_map(DB) ->
    maps:put(Sock, Info, DB).

sock_which_monitors(DB, Sock) ->
    case sock_lookup(DB, Sock) of
	{value, #info{mons = Mons}} ->
	    Mons;
	false ->
	    []
    end.

sock_size(DB, Sock) ->
    length(sock_which_monitors(DB, Sock)).
    

%% --- Users utility functions ---

user_delete(DB, Pid) when is_map(DB) ->
    maps:remove(Pid, DB).

user_lookup(DB, Pid) when is_map(DB) ->
    lookup(DB, Pid).

user_take(DB, Pid) when is_map(DB) ->
    maps:take(Pid, DB).

user_update(DB, Pid, Value) when is_map(DB) andalso is_pid(Pid) ->
    maps:put(Pid, Value, DB).

user_which_monitors(DB, Pid) when is_map(DB) andalso is_pid(Pid) ->
    case user_lookup(DB, Pid) of
	{value, #user{mons = Mons}} ->
	    Mons;
	false ->
	    []
    end.

user_size(DB, Pid) when is_map(DB) andalso is_pid(Pid) ->
    length(user_which_monitors(DB, Pid)).


%% =========================================================================

lookup(DB, Key) ->
    case maps:get(Key, DB, ?NOT_FOUND) of
	?NOT_FOUND ->
	    false;
	Value ->
	    {value, Value}
    end.

    
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
