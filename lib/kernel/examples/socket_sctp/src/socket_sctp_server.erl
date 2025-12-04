%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

%% ========================================================================
%%
%% This is a simple example of an echo server.
%% This is *not* an example of how to write an application.
%% Its simply examplifies what kind of messages needs to
%% be handled for an server using socket SCTP.
%%
%% The example consists of three parts:
%% 1) Server starter:
%%    Starts the acceptor.
%% 2) Acceptor:
%%    Listens for "connect messages" and when one arrives
%%    spawns a handler process.
%% 3) Handler:
%%    The handler performs a peeloff, and handles all communication
%%    on the new socket.
%%
%% ========================================================================

-module(socket_sctp_server).

-export([start/0, start/1, start/2,
         start_monitor/2,
         stop/1]).

-export([start_it/2]).

-include("socket_sctp_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DEFAULT_OPTS, #{domain => inet, debug => false}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type opts() :: #{debug  := boolean(),
                  domain := inet | inet6}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(node(), ?DEFAULT_OPTS).

%% This is when started from a shell: 
%% erl -s socket_sctp_server start
%% or
%% erl -run socket_sctp_server start
start([]) ->
    start(node(), ?DEFAULT_OPTS);

%% This is when started from a shell: 
%% erl -s socket_sctp_server start true
start([Debug]) when is_boolean(Debug) ->
    start(node(), ensure_opts(#{debug => Debug}));

%% This is when started from a shell: 
%% erl -run socket_sctp_server start true
start([DebugStr]) when is_list(DebugStr) ->
    Debug =
       try list_to_atom(DebugStr) of
           Dbg when is_boolean(Dbg) ->
	       Dbg;
	   _ ->
	       ?ERROR("Invalid debug argument: '~s'", [DebugStr]),
	       ?STOP()
	   catch
	       _:_:_ ->
		   ?ERROR("Failed convert debug string ('~s')",
                          [DebugStr]),
		   ?STOP()
	   end,
    start(node(), ensure_opts(#{debug => Debug}));

start(Debug) when is_boolean(Debug) ->
    start(node(), ensure_opts(#{debug => Debug}));

start(Opts) when is_map(Opts) ->
    start(node(), ensure_opts(Opts));

start(Invalid) ->
    ?ERROR("invalid start command: "
           "~n   Invalid: ~p"
           "~n", [Invalid]),
    ?STOP().



-spec start(Node, Opts) -> {ok, Server} | {error, Reason} when
      Node   :: node(),
      Opts   :: opts(),
      Server :: {Pid, SA},
      Pid    :: pid(),
      SA     :: socket:sockaddr(),
      Reason :: term().

start(Node, Opts) ->
    do_start(Node, ensure_opts(Opts)).


-spec start_monitor(Node, Opts) -> {ok, Server} | {error, Reason} when
      Node   :: node(),
      Opts   :: opts(),
      Server :: {Pid, MRef, SA},
      Pid    :: pid(),
      MRef   :: reference(),
      SA     :: socket:sockaddr(),
      Reason :: term().

start_monitor(Node, Opts) when is_atom(Node) andalso is_map(Opts) ->
    case do_start(Node, ensure_opts(Opts)) of
        {ok, {Pid, SA}} ->
            MRef = erlang:monitor(process, Pid),
            {ok, {Pid, MRef, SA}};
        {error, _} = ERROR ->
            ERROR
    end.


do_start(Node,
         #{domain := Domain,
           debug  := Debug} = Opts)
  when (Node =/= node()) andalso
       ((Domain =:= inet) orelse (Domain =:= inet6)) andalso
       is_boolean(Debug) ->
    Args = [self(), Opts],
    case rpc:call(Node, ?MODULE, start_it, Args) of
        {badrpc, _} = Reason ->
            {error, Reason};
        {ok, {Pid, _}} = OK when is_pid(Pid) ->
            OK;
        {error, _} = ERROR ->
            ERROR
    end;
do_start(_,
         #{domain := Domain,
           debug  := Debug} = Opts)
  when ((Domain =:= inet) orelse (Domain =:= inet6)) andalso
       is_boolean(Debug) ->
    case start_it(self(), Opts) of
        {ok, {Pid, _}} = OK when is_pid(Pid) ->
            OK;
        {error, _} = ERROR ->
            ERROR
    end.


start_it(Parent, #{domain := Domain} = Opts) when is_pid(Parent) ->
    case ?WHICH_ADDR(Domain) of
        {ok, Addr} ->
            start_acceptor(Opts#{parent => Parent,
                                 ip     => Addr});
        {error, _} = ERROR ->
            ERROR
    end.


%% ---

stop(Pid) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {?MODULE, self(), stop},
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    end.



            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_acceptor(#{debug := Debug} = Opts) ->

    put(sname, "server-starter"),
    put(dbg, Debug),

    ?DEBUG("~s -> entry", [?FUNCTION_NAME]),

    Self = self(),
    {Acceptor, MRef} =
        spawn_monitor(fun() ->
                              acceptor_init(Opts#{starter => Self})
                      end),
    receive
        {'DOWN', MRef, process, Acceptor, Reason} ->
            ?ERROR("Received unexpected DOWN from starting acceptor:"
                   "~n   ~p", [Reason]),
            {error, {acceptor_start, Reason}};

        {?MODULE, Acceptor, {started, SockAddr}} ->
            ?INFO("acceptor started:"
                  "~n   Acceptor: ~p"
                  "~n   SockAddr: ~p", [Acceptor, SockAddr]),
            erlang:demonitor(MRef),
            {ok, {Acceptor, SockAddr}}
    end.

acceptor_init(#{parent  := Parent,
                starter := Starter,
                domain  := Domain,
                ip      := IP,
                debug   := Debug}) ->

    put(sname, "acceptor"),
    put(dbg,   Debug),

    ?DEBUG("~s -> entry with"
           "~n   Parent:  ~p"
           "~n   Starter: ~p"
           "~n   IP:      ~p", [?FUNCTION_NAME, Parent, Starter, IP]),

    ?DEBUG("~s -> open socket (with Domain = ~w)", [?FUNCTION_NAME, Domain]),
    Sock =
	case socket:open(Domain, seqpacket, sctp) of
	    {ok, S} ->
		S;
	    {error, OReason} ->
		?ERROR("Failed open socket: "
                       "~n   ~p", [OReason]),
		?STOP()
	end,

    RawSA = #{family => Domain,
              addr   => IP,
              port   => 0},
    ?DEBUG("bind socket to: "
           "~n   ~p", [RawSA]),
    case socket:bind(Sock, RawSA) of
	ok ->
	    ok;
	{error, BReason} ->
	    ?ERROR("Failed bind socket: "
	           "~n   RawSA: ~p"
                   "~n   ~p", [RawSA, BReason]),
	    ?STOP()
    end,

    ?DEBUG("listen"),
    case socket:listen(Sock, true) of
	ok ->
	    ok;
	{error, LReason} ->
	    ?ERROR("Failed listen: "
                   "~n   ~p", [LReason]),
	    ?STOP()
    end,

    Evs = ?SCTP_EVENTS(false),
    ?DEBUG("subscribe to (sctp) events"),
    case socket:setopt(Sock, {sctp, events}, Evs) of
        ok ->
            ok;
        {error, Reason4} ->
            ?ERROR("Failed SCTP events:"
                   "~n   ~p", [Reason4]),
            ?STOP()
    end,

    ?DEBUG("monitor parent"),
    MRef = erlang:monitor(process, Parent),

    ?DEBUG("get sockname"),
    SockAddr = #{addr := BAddr, port := BPort} =
        case socket:sockname(Sock) of
            {ok, SA} ->
                SA;
            {error, SNReason} ->
                ?ERROR("Failed get sockname: "
                       "~n   ~p", [SNReason]),
                ?STOP()
        end,

    ?INFO("Socket bound to: "
          "~n   Addr: ~s (~p)"
          "~n   Port: ~w", [inet:ntoa(BAddr), BAddr, BPort]),
    Starter ! {?MODULE, self(), {started, SockAddr}},

    ?INFO("init done"),
    acceptor_loop(#{parent      => Parent,
                    parent_mref => MRef,
                    sock        => Sock,
                    addr        => BAddr,
                    port        => BPort,
                    select      => undefined,
		    %% Next handler ID, should wrap but we ignore this
		    handler_id  => 1,
                    connections => #{}}).


acceptor_loop(#{parent      := Parent,
                sock        := Sock,
                select      := undefined,
		handler_id  := NextHandlerID,
                connections := Connections} = State) ->
    ?DEBUG("~s -> try accept connection (recv)", [?FUNCTION_NAME]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := Flags} = Msg} ->
            ?DEBUG("~s -> received message - verify connection attempt",
	           [?FUNCTION_NAME]),
            case lists:member(notification, Flags) of
                true ->
                    ?DEBUG("~s -> is notification - make sure its comm-up",
		           [?FUNCTION_NAME]),
                    case Msg of
                        #{notification :=
                              #{type          := assoc_change,
                                state         := comm_up,
                                assoc_id      := AID,
                                '$esock_name' := sctp_notification}} ->
                            %% Create handler
                            ?INFO("Received assoc-change:comm-up "
                                  "notification regarding assoc ~w", [AID]),
			    case handler_start(NextHandlerID,
			    	               Parent, Sock, AID) of
                                {Handler, HMRef} when is_pid(Handler) ->
                                    NewConnections =
                                        Connections#{Handler => #{id       => NextHandlerID,
					                          mref     => HMRef,
                                                                  assoc_id => AID},
                                                     NextHandlerID => Handler},
                                    ?DEBUG("~s -> handler started:"
                                           "~n   Connections:    ~p"
                                           "~n   NewConnections: ~p",
                                           [?FUNCTION_NAME,
                                            Connections, NewConnections]),
                                    NewState = State#{handler_id  => NextHandlerID + 1,
				                      connections => NewConnections},
                                    acceptor_loop(NewState);
				{error, Reason} ->
				    ?WARNING("Failed starting handler: "
				             "~n   ~p", [Reason]),
			            acceptor_loop(State)
			    end;
                        #{notification := Notif} ->
                            ?WARNING("Received unexpected notification: "
                                     "~n   ~p", [Notif]),
                            acceptor_loop(State);
                        _ ->
                            ?WARNING("Received unexpected notication msg:"
                                     "~n   ~p", [Msg]),
                            acceptor_loop(State)
                    end;
                false ->
                    ?WARNING("Received unexpected msg:"
                             "~n   ~p", [Msg]),
                    acceptor_loop(State)
            end;

        {select, SelectInfo} ->
            ?DEBUG("~s -> received select message", [?FUNCTION_NAME]),
            acceptor_loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?ERROR("recvmsg failed (accept):"
                   "~n   ~p", [Reason]),
            exit({recvmsg, accept, Reason})
    end;
acceptor_loop(#{parent      := Parent,
                parent_mref := ParentMRef,
                sock        := Sock,
                select      := {select_info, _, SelectHandle},
                connections := Connections} = State) ->
    ?DEBUG("~s -> await select message (for accept)", [?FUNCTION_NAME]),
    receive
        {?MODULE, Parent, stop} ->
            ?INFO("~s -> received 'stop' command (from parent ~p)",
                  [?FUNCTION_NAME, Parent]),
            acceptor_stop_all_handlers(Connections),
            ?DEBUG("~s -> close socket when:"
                   "~n   Socket Info: ~p",
                   [?FUNCTION_NAME, (catch socket:info(Sock))]),
            (catch socket:close(Sock)),
            ?DEBUG("~s -> stopped", [?FUNCTION_NAME]),
            exit(normal);
            
        {'DOWN', ParentMRef, process, Parent, Info} ->
            ?ERROR("Received unexpected DOWN from parent "
                   "when awaiting connection:"
                   "~n   ~p", [Info]),
            acceptor_stop_all_handlers(Connections),
            (catch socket:close(Sock)),
            exit({parent_died, Info});

        {'DOWN', MRef, process, Pid, Info} ->
            ?DEBUG("~s -> received DOWN message:"
                   "~n   MRef: ~p"
                   "~n   Pid:  ~p"
                   "~n   Info: ~p"
                   "~nwhen"
                   "~n   Connections: ~p",
                   [?FUNCTION_NAME, MRef, Pid, Info, Connections]),
            NewConnections =
                acceptor_handle_down(Connections, Pid, MRef, Info),
            NewState = State#{connections => NewConnections},
            acceptor_loop(NewState);

        {'$socket', Sock, select, SelectHandle} ->
            ?DEBUG("~s -> received select message", [?FUNCTION_NAME]),
            acceptor_loop(State#{select => undefined})

    end.


acceptor_stop_all_handlers(Connections) ->
    F = fun(Handler, #{assoc_id := AID}) when is_pid(Handler) ->
                ?WARNING("terminate handler ~p (~w)", [Handler, AID]),
                Handler ! {msg, self(), stop},
                ok;
           (_Key, _Value) -> % AID -> Handler
                ignore
        end,
    maps:foreach(F, Connections).


acceptor_handle_down(Connections,
                     Handler, _MRef, Info) ->
    case Connections of
        #{Handler := #{id := HandlerID, assoc_id := AID}} ->
            maybe_inform(Handler, HandlerID, AID, Info),
            C1 = maps:remove(Handler, Connections),
            maps:remove(AID, C1);
        _ ->
            ?WARNING("Received 'DOWN' from unknown process ~p:"
                     "~n   ~p", [Handler, Info]),
            Connections
    end.

maybe_inform(Handler, HandlerID, AID, normal) ->
    ?INFO("Handler ~w (~p) handling assoc ~w terminated normally",
          [HandlerID, Handler, AID]),
    ok;
maybe_inform(Handler, HandlerID, AID, Reason) ->
    ?WARNING("Received unexpected 'DOWN' message from Handler ~w (~p) (~w):"
             "~n   ~p", [HandlerID, Handler, AID, Reason]),
    ok.


%% ==================================================================
%%
%% Note that on some platforms (Solaris) the Assoc ID is basically
%% not useful *after* a peeloff. Instead, we use 0 (or whatever is
%% assigned in the message).
%%

handler_start(HandlerID,
              Parent, LSock, AID) ->
    Self  = self(),
    Debug = get(dbg),
    {Handler, MRef} =
        spawn_monitor(fun() ->
                              handler_init(HandlerID,
			                   #{parent => Self,
                                             lsock  => LSock,
                                             aid    => AID,
                                             debug  => Debug})
                      end),
    receive
        {'DOWN', _PMRef, process, Parent, Reason} ->
            ?ERROR("received unexpected down from parent:"
                   "~n   ~p", [Reason]),
            exit({parent, Reason});

        {'DOWN', MRef, process, Handler, Reason} ->
            ?ERROR("Received unexpected DOWN from starting handler:"
                  "~n   ~p", [Reason]),
            %% How to shut down an association
            {error, {handler_start, Reason}};

        {?MODULE, Handler, started} ->
            ?INFO("handler started:"
                  "~n   Handler: ~p", [Handler]),
            {Handler, MRef}
    end.
    
    
handler_init(ID,
             #{parent := Parent,
               lsock  := Sock,
               aid    := AID,
               debug  := Debug}) ->

    put(sname, ?F("handler[~w,~w]", [ID,AID])),
    put(dbg,   Debug),

    Evs = ?SCTP_EVENTS(true),

    ?DEBUG("~s -> try peeloff", [?FUNCTION_NAME]),
    InheritOpts = [{ip,     tos},
                   {ip,     ttl},
		   {sctp,   nodelay},
                   {socket, linger},
		   {socket, reuseaddr}],
    NewSock =
        case socket:peeloff(Sock, AID, InheritOpts) of
            {ok, S} ->
                ?DEBUG("~s(~w) -> peel off success:"
                       "~n   ~p", [?FUNCTION_NAME, AID, S]),
                S;
	    {ok, S, InheritErrs} ->
                ?WARNING("Peel off failed inherit options:"
                         "~n   ~p", [InheritErrs]),
	        S;
            {error, Reason1} ->
                ?ERROR("Peel off failure:"
                       "~n   ~p", [Reason1]),
                exit({peeloff, Reason1})
        end,

    ?DEBUG("~s -> set events option", [?FUNCTION_NAME]),
    ok = socket:setopt(NewSock, ?MK_SCTP_SOCKOPT(events), Evs),
        
    ?DEBUG("~s -> monitor processes", [?FUNCTION_NAME]),
    MRef = erlang:monitor(process, Parent, []),

    ?DEBUG("~s -> inform parent started", [?FUNCTION_NAME]),
    Parent ! {?MODULE, self(), started},

    ?DEBUG("~s -> started", [?FUNCTION_NAME]),
    handler_loop(#{sock        => NewSock,
                   assoc_id    => AID,
                   parent      => Parent,
                   parent_mref => MRef,
                   select      => undefined,
                   shutdown    => undefined}).
	    

handler_loop(#{sock        := Sock,
               assoc_id    := undefined,
               shutdown    := complete} = _State) ->
    ?DEBUG("~s -> shutdown complete", [?FUNCTION_NAME]),
    (catch socket:close(Sock)),
    exit(normal);

handler_loop(#{sock     := Sock,
               assoc_id := AssignedAID,
               select   := undefined,
               shutdown := Shutdown} = State)
  when (AssignedAID =/= undefined) ->
    ?DEBUG("~s -> try recv when"
           "~n   Shutdown: ~w"
           "~n   Status:   ~p",
           [?FUNCTION_NAME, Shutdown, ?WHICH_STATUS(Sock, AssignedAID)]),
    case socket:recvmsg(Sock, nowait) of
        {ok, #{flags := _,
               addr  := SA,
               iov   := [Data],
               ctrl  := [#{level := sctp,
                           type  := sndrcv,
                           value := #{assoc_id := AID,
                                      stream   := Stream}}]}} ->
            ?DEBUG("~s -> received data message: "
                   "~n   From:     ~p"
                   "~n   Assoc ID: ~p"
                   "~n   Stream:   ~p",
		   [?FUNCTION_NAME, SA, AID, Stream]),
            handler_handle_data(Sock, AID, Stream, Data),
            handler_loop(State);

	%% On some platforms (Solaris) we may not get the SRI
	%% How do we know which stream? Just pick one?
        {ok, #{flags := _,
               addr  := SA,
               iov   := [_Data],
               ctrl  := []}} ->
            ?WARNING("Received data message without SRI from (ignore): "
                   "~n   ~p", [SA]),
            %% handler_handle_data(Sock, AssignedAID, 0, Data),
            handler_loop(State);

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := assoc_change,
                                 state    := shutdown_comp,
                                 assoc_id := AID}}} ->
            %% Shutdown is now complete
            ?INFO("Received shutdown-complete event (for assoc ~w)", [AID]),
            handler_loop(State#{assoc_id => undefined,
                                shutdown => complete});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := shutdown_event,
                                 assoc_id := AID}}} ->
            ?INFO("Received shutdown-event (for assoc ~w)", [AID]),
            handler_loop(State#{shutdown => 'begin'});

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := #{type     := peer_addr_change,
                                 state    := NState,
                                 assoc_id := AID}}}
          when (NState =:= addr_confirmed) orelse
               (NState =:= addr_available) ->
            ?INFO("Received peer-addr-change (for assoc ~w): ~w",
	    		    [AID, NState]),
            handler_loop(State);

        {ok, #{flags        := _, % Should contain the 'notification' flag
               notification := Notif}} ->
            ?WARNING("Received unexpected notification:"
	    	     "~n   Notification: ~p", [Notif]),
            handler_loop(State);

        {ok, Msg} ->
            ?WARNING("Received unexpected message:"
	    	     "~n   ~p", [Msg]),
            handler_loop(State);

        {select, SelectInfo} ->
            ?DEBUG("~s -> select", [?FUNCTION_NAME]),
            handler_loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?ERROR("Failed recvmsg:"
                   "~n   ~p", [Reason]),
            exit({recvmsg, Reason})
    end;
handler_loop(#{parent      := Parent,
               parent_mref := MRef,
               sock        := Sock,
               assoc_id    := AssignedAID,
               select      := {select_info, _, SelectHandle},
               shutdown    := Shutdown} = State) ->
    ?DEBUG("~s -> await select when"
           "~n   Shutdown: ~p"
           "~n   Status:   ~p",
           [?FUNCTION_NAME, Shutdown, ?WHICH_STATUS(Sock, AssignedAID)]),
    receive
        {'DOWN', MRef, process, Parent, Reason} ->
            ?ERROR("Received unexpected down from parent:"
                   "~n   Reason:  ~p", [Reason]),
            (catch socket:close(Sock)),
            exit({server, Reason});

        {'$socket', Sock, select, SelectHandle} ->
            ?DEBUG("~s -> received select message", [?FUNCTION_NAME]),
            handler_loop(State#{select => undefined});

        {?MODULE, Parent, terminate} ->
            ?INFO("Received terminate request"),
            (catch socket:close(Sock)),
            exit(normal)
    end.

    
handler_handle_data(Sock, AID, Stream, Data)
  when (Sock =/= undefined) andalso
       (AID  =/= undefined) andalso
       is_binary(Data) ->
    ?INFO("RECV: ~p", [binary_to_list(Data)]),
    SRI     = #{assoc_id => AID, stream => Stream},
    CtrlSRI = #{level => sctp,
                type  => sndrcv,
                value => SRI},
    Msg = #{iov  => [Data],
            ctrl => [CtrlSRI]},
    case socket:sendmsg(Sock, Msg) of
        ok ->
            ?INFO("data echo done"),
            ok;
        {error, Reason} ->
            ?ERROR("failed sending (echo) data back: "
                   "~n   ~p", [Reason]),
            exit({sendmsg, Reason})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_opts(Opts) ->
    maps:merge(?DEFAULT_OPTS, Opts).

