%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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

-module(socket_sctp_traffic_server).

-include("socket_sctp_traffic_lib.hrl").

-export([
         start/0, start/1, start/2, start/3,
         start_monitor/0, start_monitor/1, start_monitor/2, start_monitor/3,
         stop/1
        ]).

-export([start_it/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MSG(Pid, Info), {?MODULE, (Pid), (Info)}).

-define(TIMEOUT,        5000).

-define(DEFAULT_OPTS, #{domain   => inet,
                        debug    => false,
                        %% Threaded:
                        %%   true:  The server spawns a handler for every
                        %%          connection (who then perform peeloff)
                        %%   false: The server handles all traffic itself.
                        threaded => true}).
-define(STATUS_TIMEOUT, 5000).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(node(), 0, #{}).

start(PortNo) when is_integer(PortNo) andalso (PortNo >= 0) ->
    start(node(), PortNo, #{});
start(Opts) when is_map(Opts) ->
    start(node(), 0, Opts);
start(Node) when is_atom(Node) ->
    start(Node, 0, #{}).

start(PortNo, Opts)
  when is_integer(PortNo) andalso (PortNo >= 0) andalso
       is_map(Opts) ->
    start(node(), PortNo, Opts);
start(Node, PortNo)
  when is_atom(Node) andalso
       is_integer(PortNo) andalso (PortNo >= 0) ->
    start(Node, PortNo, #{});
start(Node, Opts)
  when is_atom(Node) andalso
       is_map(Opts) ->
    start(Node, 0, Opts).

start(Node, PortNo, Opts)
  when is_atom(Node) andalso
       is_integer(PortNo) andalso (PortNo >= 0) andalso
       is_map(Opts) ->
    do_start(Node, PortNo, ensure_opts(Opts)).


start_monitor() ->
    start_monitor(node(), 0, #{}).

start_monitor(PortNo) when is_integer(PortNo) andalso (PortNo >= 0) ->
    start_monitor(node(), PortNo, #{});
start_monitor(Opts) when is_map(Opts) ->
    start_monitor(node(), 0, Opts);
start_monitor(Node) when is_atom(Node) ->
    start_monitor(Node, 0, #{}).

start_monitor(PortNo, Opts)
  when is_integer(PortNo) andalso (PortNo >= 0) andalso
       is_map(Opts) ->
    start_monitor(node(), PortNo, Opts);
start_monitor(Node, PortNo)
  when is_atom(Node) andalso
       is_integer(PortNo) andalso (PortNo >= 0) ->
    start_monitor(Node, PortNo, #{});
start_monitor(Node, Opts)
  when is_atom(Node) andalso
       is_map(Opts) ->
    start_monitor(Node, 0, Opts).

start_monitor(Node, PortNo, Opts)
  when is_atom(Node) andalso
       is_integer(PortNo) andalso (PortNo >= 0) andalso
       is_map(Opts) ->
    case do_start(Node, PortNo, ensure_opts(Opts)) of
        {ok, {Pid, SA}} ->
            MRef = erlang:monitor(process, Pid),
            {ok, {Pid, MRef, SA}};
        {error, _} = ERROR ->
            ERROR
    end.



do_start(Node, PortNo, Opts)
  when (Node =/= node()) ->
    Args = [self(), PortNo, Opts],
    case rpc:call(Node, ?MODULE, start_it, Args) of
        {badrpc, _} = Reason ->
            {error, Reason};
        {ok, {Pid, _}} = OK when is_pid(Pid) ->
            OK;
        {error, _} = ERROR ->
            ERROR
    end;
do_start(_, PortNo, Opts) ->
    case start_it(self(), PortNo, Opts) of
        {ok, {Pid, _}} = OK when is_pid(Pid) ->
            OK;
        {error, _} = ERROR ->
            ERROR
    end.



start_it(Parent, PortNo, Opts) when is_pid(Parent) ->

    put(sname, "server-starter"),
    set_debug(Opts),

    ?DBG("~s -> entry", [?FUNCTION_NAME]),

    Self             = self(),
    {Pid, MRef} =
        spawn_monitor(fun() ->
                              init(Opts#{starter => Self,
                                         parent  => Parent,
                                         port    => PortNo})
                      end),
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            ?ERROR("Received unexpected DOWN from starting acceptor:"
                   "~n   ~p", [Reason]),
            erase(sname),
            {error, Reason};
        
        ?MSG(Pid, {started, SockAddr}) ->
            ?INFO("acceptor started:"
                  "~n   Pid:      ~p"
                  "~n   SockAddr: ~p", [Pid, SockAddr]),
            erlang:demonitor(MRef),
            erase(sname),
            {ok, {Pid, SockAddr}}
    end.
        

stop(Pid) when is_pid(Pid) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! ?MSG(self(), stop),
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    after ?TIMEOUT ->
            ?ERROR("Server stop timeout"),
            erlang:demonitor(MRef),
            exit(Pid, kill),
            ok
    end.

        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                  
init(#{starter  := Starter,
       parent   := Parent,
       domain   := Domain,
       port     := PortNo,
       threaded := Threaded} = State) ->

    set_debug(State),
    ?SET_SNAME("server"),

    ?DBG("try find local (~w) addr", [Domain]),
    Addr =
        case ?WHICH_LOCAL_ADDR(Domain) of
            {ok, A} ->
                ?DBG("A: ~p", [A]),
                A;
            {error, AReason} ->
                ?ERROR("Failed find local addr:"
                       "~n   ~p", [AReason]),
                exit({local_addr, AReason})
        end,

    ?DBG("try open"),
    Sock = case socket:open(Domain, seqpacket, sctp) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   ?ERROR("Failed open:"
                          "~n   ~p", [OReason]),
                   exit({open_failed, OReason})
           end,

    SockAddr0 = #{family => Domain,
                  addr   => Addr,
                  port   => PortNo},
    ?DBG("try bind to:"
         "~n   ~p", [SockAddr0]),
    case socket:bind(Sock, SockAddr0) of
        ok ->
            ok;
        {error, BReason} ->
            ?ERROR("Failed bind:"
                   "~n   ~p", [BReason]),
            exit({bind_failed, BReason})
    end,


    ?DBG("try make listen socket"),
    case socket:listen(Sock, true) of
        ok ->
            ok;
        {error, LReason} ->
            ?ERROR("Failed listen:"
                   "~n   ~p", [LReason]),
            exit({listen, LReason})
    end,

    ?DBG("try set 'events' (sctp) socket option"),
    case socket:setopt(Sock,
                       ?MK_SCTP_SOCKOPT(events),
                       ?SCTP_EVENTS(not Threaded)) of
        ok ->
            ok;
        {error, SReason} ->
            ?ERROR("Failed setopt events:"
                   "~n   ~p", [SReason]),
            exit({setopt_events, SReason})
    end,

    ?DBG("try sockname (get port number)"),
    SockAddr =
        case socket:sockname(Sock) of
            {ok, SA} ->
                SA;
            {error, NReason} ->
                ?ERROR("Failed sockname:"
                       "~n   ~p", [NReason]),
                exit({sockname, NReason})
        end,

    ?DBG("monitor parent"),
    MRef = erlang:monitor(process, Parent),

    ?DBG("inform parent (sockaddr: ~p)", [SockAddr]),
    Starter ! ?MSG(self(), {started, SockAddr}),

    ?INFO("started"),
    loop(#{parent      => Parent,
           parent_mref => MRef,
           sock        => Sock,
           connections => #{},
           select      => undefined,
           %% Only use the id if threaded
           id          => 1,
           threaded    => Threaded}).
    

loop(#{sock   := Sock,
       select := undefined} = State) when (Sock =/= undefined) ->
    ?DBG("try recv"),
    case socket:recvmsg(Sock, nowait) of
        {ok, Msg} ->
            NewState = handle_msg(State, Msg),
            loop(NewState);

        {select, SelectInfo} ->
            loop(State#{select => SelectInfo});

        {error, Reason} ->
            ?ERROR("Failed recvmsg: "
                   "~n   ~p", [Reason]),
            exit({recvmsg, Reason})
    end;

loop(#{parent      := Parent,
       parent_mref := MRef,
       sock        := Sock,
       select      := {select_info, _, SelectHandle} = SelectInfo} = State) ->
    ?DBG("await select message"),
    receive
        {'$socket', Sock, select, SelectHandle} ->
            ?DBG("received select message"),
            loop(State#{select => undefined});

        {'DOWN', MRef, process, Parent, Info} ->
            ?ERROR("Received unexpected DOWN from parent:"
                   "~n   ~p", [Info]),
            (catch socket:cancel(Sock, SelectInfo)),
            (catch socket:close(Sock)),
            exit({parent, Info});

        ?MSG(Parent, stop) ->
            ?INFO("Received stop request"),
            (catch socket:cancel(Sock, SelectInfo)),
            ?INFO("close socket"),
            (catch socket:close(Sock)),
            ?INFO("stop done"),
            exit(normal)
    end.

                
handle_msg(State,
           #{notification := Notif}) ->
    handle_notification(State, Notif);

handle_msg(#{threaded := false} = State,
           #{iov  := [Data],
             ctrl := [#{level := sctp,
                        type  := sndrcv,
                        value := #{assoc_id := AssocID,
                                   stream   := Stream}}]}) ->
    handle_data(State, Data, AssocID, Stream);
handle_msg(_State, Msg) ->
    ?ERROR("Received unexpected message:"
           "~n   ~p", [Msg]),
    exit(unexpected_msg).


handle_notification(#{threaded    := false = _Threaded,
                      connections := Connections} = State,
                    #{type     := assoc_change,
                      state    := comm_up,
                      assoc_id := AssocID}) ->
    ?INFO("Received assoc-change(comm-up) for assoc ~w", [AssocID]),
    NewConnections = Connections#{AssocID => #{state => comm_up}},
    State#{connections => NewConnections};
handle_notification(#{sock        := Sock,
                      threaded    := true = _Threaded,
                      connections := Connections,
                      id          := ID} = State,
                    #{type     := assoc_change,
                      state    := comm_up,
                      assoc_id := AssocID}) ->
    ?INFO("Received assoc-change(comm-up) for assoc ~w", [AssocID]),
    {Pid, MRef}    = handler_start(State, ID, Sock, AssocID),
    NewConnections = Connections#{AssocID => #{state    => comm_up,
                                               handler  => Pid},
                                  Pid     => #{id       => ID,
                                               mref     => MRef,
                                               assoc_id => AssocID}},
    State#{connections => NewConnections,
           id          => ID + 1};
%% Is this just a race?
handle_notification(#{connections := Connections} = State,
                    #{type     := peer_addr_change,
                      state    := addr_available,
                      assoc_id := AssocID}) ->
    case Connections of
        #{AssocID := _} ->
            ?INFO("Received expected peer-addr-change(available) for assoc ~w",
                  [AssocID]),
            State;
        _ ->
            ?INFO("Received peer-addr-change(available) for unknown assoc ~w",
                  [AssocID]),
            State
    end;
handle_notification(#{connections := Connections} = State,
                    #{type     := peer_addr_change,
                      state    := addr_confirmed,
                      assoc_id := AssocID}) ->
    case Connections of
        #{AssocID := _} ->
            ?INFO("Received peer-addr-change(confirmed) for assoc ~w",
                  [AssocID]),
            State;
        _ ->
            ?WARNING("Received peer-addr-change(confirmed) "
                     "for unknown assoc ~w", [AssocID]),
            State
    end;
handle_notification(#{threaded    := false,
                      connections := Connections} = State,
                    #{type     := shutdown_event,
                      assoc_id := AssocID}) ->
    case Connections of
        #{AssocID := Conn} ->
            ?INFO("Received shutdown event for assoc ~w", [AssocID]),
            NewConn        = Conn#{state => shutdown_begin},
            NewConnections = Connections#{AssocID => NewConn},
            State#{connections => NewConnections};
        _ ->
            ?WARNING("Received shutdown event for unknown assoc ~w", [AssocID]),
            State
    end;
handle_notification(#{threaded    := false,
                      connections := Connections} = State,
                    #{type     := assoc_change,
                      state    := shutdown_comp,
                      assoc_id := AssocID}) ->
    case Connections of
        #{AssocID := Conn} ->
            %% We should really delete the assoc here
            ?INFO("Received assoc-change(shutdown-complete) event for assoc ~w",
                  [AssocID]),
            NewConn        = Conn#{state => shutdown_complete},
            NewConnections = Connections#{AssocID => NewConn},
            State#{connections => NewConnections};
        _ ->
            ?WARNING("Received assoc-change(shutdown-complete) "
                     "event for unknown assoc ~w", [AssocID]),
            State
    end;

handle_notification(#{threaded    := false,
                      connections := Connections} = State,
                    #{type     := assoc_change,
                      state    := comm_lost,
                      assoc_id := AssocID,
                      error    := Error}) ->
    case Connections of
        #{AssocID := Conn} ->
            %% We should really delete the assoc here
            ?INFO("Received assoc-change(comm-lost) event for assoc ~w",
                  [AssocID]),
            NewConn        = Conn#{state => comm_lost},
            NewConnections = Connections#{AssocID => NewConn},
            State#{connections => NewConnections};
        _ ->
            ?WARNING("Received assoc-change(comm-lost) "
                     "event for unknown assoc ~w:"
                     "~n   Error: ~w", [AssocID, Error]),
            State
    end;

handle_notification(#{sock := Sock} = _State, Notif) ->
    ?ERROR("Received unexpected notification:"
           "~n   ~p", [Notif]),
    (catch socket:close(Sock)),
    exit(unexpected_notification).


%% ========================================================================

handler_start(#{parent      := Parent,
                parent_mref := PMRef} = State0, ID, Sock, AssocID) ->
    
    Self   = self(),
    State1 = maps:remove(threaded,    State0),
    State2 = maps:remove(connections, State1),
    State3 = State2#{select => undefined},
    Handler = {Pid, MRef} =
        erlang:spawn_monitor(
          fun() ->
                  handler_init(State3, ID, Sock, AssocID, Self)
          end),
    receive
        ?MSG(Pid, started) ->
            ?DBG("Handler started"),
            Handler;

        {'DOWN', PMRef, process, Parent, Reason} ->
            ?ERROR("Received unexpected down from parent:"
                   "~n   ~p", [Reason]),
            exit({parent, Reason});

        {'DOWN', MRef, process, Pid, Reason} ->
            ?ERROR("Received unexpected down from handler:"
                   "~n   ~p", [Reason]),
            exit({handler, AssocID, Reason})

    end.


handler_init(State, ID, Sock, AssocID, Parent) ->
    
    set_debug(State),
    ?SET_SNAME(?F("handler[~w,~w]", [ID, AssocID])),
    
    ?DBG("~s -> entry - try peeloff", [?FUNCTION_NAME]),

    NewSock =
        case socket:peeloff(Sock, AssocID) of
            {ok, S} ->
                S;
            {error, Reason} ->
                ?ERROR("Failure peeloff:"
                       "~n   ~p", [Reason]),
            exit({peeloff, Reason})
        end,
    
    ?DBG("inform parent started"),
    Parent ! ?MSG(self(), started),
    
    ?DBG("monitor parent"),
    MRef = erlang:monitor(process, Parent, []),

    ?INFO("started"),
    handler_loop(State#{id          => ID,
                        parent      => Parent,
                        parent_mref => MRef,
                        sock        => NewSock,
                        assoc_id    => AssocID,
                        select      => undefined}).


handler_loop(#{sock   := Sock,
               select := undefined} = State) ->
    case socket:recvmsg(Sock, nowait) of
        {ok, Msg} ->
            NewState = handler_handle_msg(State, Msg),
            handler_loop(NewState);

        {select, SelectInfo} ->
            ?DBG("select"),
            handler_loop(State#{select => SelectInfo});

        {error, closed} ->
            ?INFO("Socket closed - terminating"),
            exit(normal);

        {error, Reason} ->
            ?ERROR("Failed recvmsg:"
                   "~n   ~p", [Reason]),
            exit({recvmsg, Reason})
    end;

handler_loop(
  #{parent      := Parent,
    parent_mref := MRef,
    sock        := Sock,
    select      := {select_info, _, SelectHandle} = SelectInfo} = State) ->
    ?DBG("await select"),
    receive
        {'$socket', Sock, select, SelectHandle} ->
            ?DBG("Received select message"),
            handler_loop(State#{select => undefined});

        {'$socket', Sock, abort, Info} ->
            ?WARNING("Received unexpected abort message: "
                     "~n   ~p", [Info]),
            handler_loop(State#{select => undefined});

        {'DOWN', MRef, process, Parent, Reason} ->
            ?ERROR("Received unexpected down from server:"
                   "~n   ~p", [Reason]),
            (catch socket:cancel(Sock, SelectInfo)),
            (catch socket:close(Sock)),
            exit({server, Reason})

    end.

handler_handle_msg(
  State,
  #{flags := _,
    addr  := _SA,
    iov   := [Data],
    ctrl  := [#{level := sctp,
                type  := sndrcv,
                value := #{assoc_id := AssocID,
                           stream   := Stream}}]}) ->
    handle_data(State, Data, AssocID, Stream);
handler_handle_msg(
  #{assoc_id := AssocID} = State,
  #{flags := _,
    addr  := _SA,
    iov   := [Data],
    ctrl  := []}) ->
    ?DBG("Received data message without SRI"),
    handle_data(State, Data, AssocID, 0);
handler_handle_msg(
  State,
  #{flags        := _, % Should contain the 'notification' flag
    notification := Notif}) ->
    handler_handle_notification(State, Notif),
    State;
handler_handle_msg(_State, Msg) ->
    ?ERROR("Received unknown message:"
           "~n   ~p", [Msg]),
    exit(unexpected_msg).

handler_handle_notification(#{sock := Sock} = _State,
                            #{type     := assoc_change,
                              state    := comm_lost,
                              assoc_id := AssocID}) ->
    ?INFO("Received assoc-change(comm-lost) for assoc ~w", [AssocID]),
    %% We should really wait for the socket close...
    (catch socket:close(Sock)),
    exit(normal);
handler_handle_notification(#{sock := Sock} = _State,
                            #{type     := assoc_change,
                              state    := shutdown_comp,
                              assoc_id := AssocID}) ->
    ?INFO("Received assoc-change(shutdown-comp) for assoc ~w", [AssocID]),
    %% We should really wait for the socket close...
    (catch socket:close(Sock)),
    exit(normal);
handler_handle_notification(State,
                            #{type     := shutdown_event,
                              assoc_id := AssocID}) ->
    ?INFO("Received shutdown-event for assoc ~w", [AssocID]),
    State#{state => shutdown};
handler_handle_notification(State,
                            #{type     := peer_addr_change,
                              state    := PState,
                              assoc_id := AssocID}) ->
    ?INFO("Received peer-addr-change(~w) for assoc ~w", [PState, AssocID]),
    State.


%% ========================================================================

handle_data(#{sock := Sock} = State,
            <<(?PROTO_TAG):32,
              (?REQUEST_TAG):32,
              Seq:32,
              PayloadSz:32,
              CheckSum:32,
              Payload:PayloadSz/binary>>,
            AssocID, Stream) ->
    case erlang:crc32(Payload) of
        CheckSum ->
            %% We should really do something more creative,
            %% but will have to do for now...
            Data    = <<(?PROTO_TAG):32,
                        (?REPLY_TAG):32,
                        Seq:32,
                        PayloadSz:32,
                        CheckSum:32,
                        Payload:PayloadSz/binary>>,
            SRI     = #{assoc_id => AssocID, stream => Stream},
            CtrlSRI = #{level => sctp,
                        type  => sndrcv,
                        value => SRI},
            Msg = #{iov  => [Data],
                    ctrl => [CtrlSRI]},
            %% We should change mode here
            %% (to send, and put the msg in a "buffer")
            %% but we make it easy on ourselves and just send...
            ok = socket:sendmsg(Sock, Msg),
            State;
        BadCheckSum ->
            exit({bad_checksum, CheckSum, BadCheckSum})
    end.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_opts(Opts) when is_map(Opts) ->
    maps:merge(?DEFAULT_OPTS, Opts).

set_debug(#{debug := Debug}) when is_boolean(Debug) ->
    ?SET_DEBUG(Debug);
set_debug(_) ->
    ?SET_DEBUG(false).

