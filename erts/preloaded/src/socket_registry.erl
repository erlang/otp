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
%%
%% =========================================================================

-module(socket_registry).

-export([
         start/0,
         number_of/0,
         which_sockets/1,
	 monitor/1,
	 demonitor/1
        ]).


-record(esock_xref,
	{
	 mref :: reference(),
	 sock :: socket:socket()
	}).

-record(esock_monitor,
	{
	 pid  :: pid(),
	 mref :: reference()
	}).

-record(esock_info,
        {
         sock       :: socket:socket(),
	 %% Add time - erlang:monotonic_time(milli_seconds)
         atime      :: integer(),
	 mons  = [] :: [esock_monitor()]
        }).

-record(state,
	{
	 db   = [] :: [esock_info()],
	 xref = [] :: [esock_xref()]
	}).

-type esock_xref()    :: #esock_xref{}.
-type esock_monitor() :: #esock_monitor{}.
-type esock_info()    :: #esock_info{}.

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

monitor(Socket) ->
    case request({monitor, Socket}) of
	{ok, MRef} ->
	    MRef;
	{error, Reason} ->
	    erlang:error(badarg, [Socket, Reason])
    end.

demonitor(MRef) ->
    %% ?DBG(MRef),
    case request({demonitor, MRef}) of
	ok ->
	    ok;
	{error, Reason} ->
	    erlang:error(badarg, [MRef, Reason])
    end.


%% =========================================================================

loop(State) ->
    receive
        {'$socket', add, Socket} ->
            loop( handle_add_socket(State, Socket) );

        {'$socket', del, Socket} ->
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
            %% erlang:display(REQ),
            {NewState, Reply} = handle_request(State, Req, From),
            reply(ReqId, From, Reply),
            loop(NewState);
        
        Msg ->
            NewState = handle_unexpected_msg(State, Msg),
            loop(NewState)
    end.


%% =========================================================================


%% =========================================================================

handle_add_socket(#state{db = DB} = State, Sock) ->
    DB2 = [#esock_info{sock = Sock, atime = timestamp()} | DB],
    State#state{db = DB2}.

handle_delete_socket(#state{db   = DB,
			    xref = XRef} = State, Sock) ->
    case db_sock_lookup(DB, Sock) of
	{value, #esock_info{mons = Mons}} ->
	    handle_send_down(Mons, Sock),
	    DB2   = db_sock_delete(DB, Sock),
	    XRef2 = xref_sock_delete(XRef, Sock),
	    State#state{db = DB2, xref = XRef2};
	false ->
	    State
    end.

handle_send_down([], _Sock) ->
    ok;
handle_send_down([#esock_monitor{pid = Pid, mref = MRef}|Mons], Sock) ->
    send_down(Pid, MRef, Sock),
    handle_send_down(Mons, Sock).

send_down(Pid, MRef, Sock) ->
    %% We do not yet have a 'reason', so use 'closed'...
    Pid ! mk_down_msg(MRef, Sock, closed).



handle_request(#state{db = DB} = State, number_of, _From) ->
    {State, db_size(DB)};

handle_request(#state{db = DB} = State, {which_sockets, Filter}, _From) ->
    {State, do_which_sockets(DB, Filter)};

handle_request(State, {monitor, Socket}, From) ->
    do_monitor_socket(State, Socket, From);

handle_request(State, {demonitor, MRef}, From) ->
    %% ?DBG({demonitor, MRef, From}),
    do_demonitor_socket(State, MRef, From);

handle_request(State, BadRequest, _From) ->
    {State, {error, {bad_request, BadRequest}}}.


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

mk_down_msg(MRef, Sock, Info) ->
    {'DOWN', MRef, socket, Sock, Info}.


%% ---

db_size(DB) ->
    length(DB).

do_which_sockets(DB, Filter) ->
    try
        [Sock ||
            #esock_info{sock = Sock} <- DB,
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
			 xref = XRefs} = State, Sock, From) ->
    case db_sock_lookup(DB, Sock) of
	{value, #esock_info{mons = Mons} = SI} ->
	    %% Check if this process has already monitored this socket
	    %% Or shall we allow multiple monitors?
	    %% That is, the same process can have multiple monitors
	    %% to the same socket?
	    case mons_pid_lookup(Mons, From) of
		{value, #esock_monitor{mref = MRef}} ->
		    Result = {ok, MRef},
		    {State, Result};
		false ->
		    MRef   = make_ref(),
		    Mon    = #esock_monitor{pid  = From,
					    mref = MRef},
		    Mons2  = [Mon|Mons],
		    SI2    = SI#esock_info{mons = Mons2},
		    DB2    = db_sock_replace(DB, Sock, SI2),
		    XRefs2 = [#esock_xref{mref = MRef, sock = Sock}|XRefs],
		    Result = {ok, MRef},
		    {State#state{db = DB2, xref = XRefs2}, Result}
	    end;
	false ->
	    Result = {error, unknown_socket},
	    {State, Result}
    end.


do_demonitor_socket(#state{db = DB, xref = XRefs} = State, MRef, From) ->
    case xref_mref_lookup(XRefs, MRef) of
	{value, #esock_xref{sock = Sock}} ->
	    %% ?DBG({xref, Sock}),
	    %% So far so good
	    case db_sock_lookup(DB, Sock) of
		%% So faar so goood
		{value, #esock_info{mons = Mons} = SI} ->
		    %% ?DBG({mons, Mons}),
		    Mons2  = mons_pid_delete(Mons, From),
		    SI2    = SI#esock_info{mons = Mons2},
		    DB2    = db_sock_replace(DB, Sock, SI2),
		    XRefs2 = xref_mref_delete(XRefs, MRef),
		    Result = ok,
		    {State#state{db = DB2, xref = XRefs2}, Result};
		false ->
		    %% The socket has been deleted => race
		    Result = {error, unknown_socket},
		    XRefs2 = xref_mref_delete(XRefs, MRef),
		    {State#state{xref = XRefs2}, Result}
	    end;
	false ->
	    %% No such monitor => race
	    Result = {error, unknown_monitor},
	    {State, Result}
    end.



%% =========================================================================

%% --- XRef utility functions ---

xref_sock_delete(XRefs, Sock) when is_list(XRefs) ->
    xref_delete(XRefs, #esock_xref.sock, Sock).

xref_mref_delete(XRefs, MRef) when is_list(XRefs) andalso is_reference(MRef) ->
    xref_delete(XRefs, #esock_xref.mref, MRef).

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
    xref_lookup(XRefs, #esock_xref.mref, MRef).

xref_lookup(XRefs, Pos, Key) ->
    lists:keysearch(Key, Pos, XRefs).



%% --- DB utility functions ---

db_sock_lookup(DB, Sock) when is_list(DB) ->
    db_lookup(DB, #esock_info.sock, Sock).

db_lookup(DB, Pos, Key) ->
    lists:keysearch(Key, Pos, DB).


db_sock_delete(DB, Sock) when is_list(DB) ->
    db_delete(DB, #esock_info.sock, Sock).

db_delete(DB, Pos, Key) ->
    lists:keydelete(Key, Pos, DB).


db_sock_replace(DB, Sock, Info) when is_list(DB) ->
    db_replace(DB, #esock_info.sock, Sock, Info).

db_replace(DB, Pos, Key, Info) ->
    lists:keyreplace(Key, Pos, DB, Info).


%% --- Monitors utility functions ---

mons_pid_delete(Mons, Pid) when is_list(Mons) ->
    mons_delete(Mons, #esock_monitor.pid, Pid).

mons_delete(Mons, Pos, Key) ->
    lists:keydelete(Key, Pos, Mons).


mons_pid_lookup(Mons, Pid) when is_list(Mons) andalso is_pid(Pid) ->
    mons_lookup(Mons, #esock_monitor.pid, Pid).

mons_lookup(Mons, Pos, Key) ->
    lists:keysearch(Key, Pos, Mons).


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
