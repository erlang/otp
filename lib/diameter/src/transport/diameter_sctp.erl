%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(diameter_sctp).

-behaviour(gen_server).

%% interface
-export([start/3]).

%% child start from supervisor
-export([start_link/1]).

%% child start from here
-export([init/1]).

%% gen_server callbacks
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include_lib("kernel/include/inet_sctp.hrl").
-include_lib("diameter/include/diameter.hrl").

-define(ERROR(T), erlang:error({T, ?MODULE, ?LINE})).

%% The default port for a listener.
-define(DEFAULT_PORT, 3868).  %% RFC 3588, ch 2.1

%% How long a listener with no associations lives before offing
%% itself.
-define(LISTENER_TIMEOUT, 30000).

%% How long to wait for a transport process to attach after
%% association establishment.
-define(ACCEPT_TIMEOUT, 5000).

-type uint() :: non_neg_integer().

%% Accepting/connecting transport process state.
-record(transport,
        {parent  :: pid(),
         mode :: {accept, pid()}
               | {connect, {list(inet:ip_address()), uint(), list()}}
                        %% {RAs, RP, Errors}
               | connect,
         socket   :: gen_sctp:sctp_socket(),
         assoc_id :: gen_sctp:assoc_id(),  %% association identifier
         peer     :: {[inet:ip_address()], uint()}, %% {RAs, RP}
         streams  :: {uint(), uint()},     %% {InStream, OutStream} counts
         os = 0   :: uint()}).             %% next output stream

%% Listener process state.
-record(listener,
        {ref       :: reference(),
         socket    :: gen_sctp:sctp_socket(),
         count = 0 :: uint(),
         tmap = ets:new(?MODULE, []) :: ets:tid(),
             %% {MRef, Pid|AssocId}, {AssocId, Pid}
         pending = {0, ets:new(?MODULE, [ordered_set])},
         tref      :: reference()}).
%% Field tmap is used to map an incoming message or event to the
%% relevent transport process. Field pending implements a queue of
%% transport processes to which an association has been assigned (at
%% comm_up and written into tmap) but for which diameter hasn't yet
%% spawned a transport process: a short-lived state of affairs as a
%% new transport is spawned as a consequence of a peer being taken up,
%% transport processes being spawned by the listener on demand. In
%% case diameter starts a transport before comm_up on a new
%% association, pending is set to an improper list with the spawned
%% transport as head and the queue as tail.

%% ---------------------------------------------------------------------------
%% # start/3
%% ---------------------------------------------------------------------------

start(T, #diameter_service{capabilities = Caps}, Opts)
  when is_list(Opts) ->
    diameter_sctp_sup:start(),  %% start supervisors on demand
    Addrs = Caps#diameter_caps.host_ip_address,
    s(T, Addrs, lists:map(fun ip/1, Opts)).

ip({ifaddr, A}) ->
    {ip, A};
ip(T) ->
    T.

%% A listener spawns transports either as a consequence of this call
%% when there is not yet an association to associate with it, or at
%% comm_up on a new association in which case the call retrieves a
%% transport from the pending queue.
s({accept, Ref} = A, Addrs, Opts) ->
    {LPid, LAs} = listener(Ref, {Opts, Addrs}),
    try gen_server:call(LPid, {A, self()}, infinity) of
        {ok, TPid} -> {ok, TPid, LAs}
    catch
        exit: Reason -> {error, Reason}
    end;
%% This implementation is due to there being no accept call in
%% gen_sctp in order to be able to accept a new association only
%% *after* an accepting transport has been spawned.

s({connect = C, _}, Addrs, Opts) ->
    diameter_sctp_sup:start_child({C, self(), Opts, Addrs}).

%% start_link/1

start_link(T) ->
    proc_lib:start_link(?MODULE,
                        init,
                        [T],
                        infinity,
                        diameter_lib:spawn_opts(server, [])).

%% ---------------------------------------------------------------------------
%% # init/1
%% ---------------------------------------------------------------------------

init(T) ->
    gen_server:enter_loop(?MODULE, [], i(T)).

%% i/1

%% A process owning a listening socket.
i({listen, Ref, {Opts, Addrs}}) ->
    {LAs, Sock} = AS = open(Addrs, Opts, ?DEFAULT_PORT),
    proc_lib:init_ack({ok, self(), LAs}),
    ok = gen_sctp:listen(Sock, true),
    true = diameter_reg:add_new({?MODULE, listener, {Ref, AS}}),
    start_timer(#listener{ref = Ref,
                          socket = Sock});

%% A connecting transport.
i({connect, Pid, Opts, Addrs}) ->
    {[As, Ps], Rest} = proplists:split(Opts, [raddr, rport]),
    RAs  = [diameter_lib:ipaddr(A) || {raddr, A} <- As],
    [RP] = [P || {rport, P} <- Ps] ++ [P || P <- [?DEFAULT_PORT], [] == Ps],
    {LAs, Sock} = open(Addrs, Rest, 0),
    proc_lib:init_ack({ok, self(), LAs}),
    erlang:monitor(process, Pid),
    #transport{parent = Pid,
               mode = {connect, connect(Sock, RAs, RP, [])},
               socket = Sock};

%% An accepting transport spawned by diameter.
i({accept, Pid, LPid, Sock}) ->
    proc_lib:init_ack({ok, self()}),
    erlang:monitor(process, Pid),
    erlang:monitor(process, LPid),
    #transport{parent = Pid,
               mode = {accept, LPid},
               socket = Sock};

%% An accepting transport spawned at association establishment.
i({accept, Ref, LPid, Sock, Id}) ->
    proc_lib:init_ack({ok, self()}),
    MRef = erlang:monitor(process, LPid),
    %% Wait for a signal that the transport has been started before
    %% processing other messages.
    receive
        {Ref, Pid} ->  %% transport started
            #transport{parent = Pid,
                       mode = {accept, LPid},
                       socket = Sock};
        {'DOWN', MRef, process, _, _} = T ->  %% listener down
            close(Sock, Id),
            x(T)
    after ?ACCEPT_TIMEOUT ->
            close(Sock, Id),
            x(timeout)
    end.

%% close/2

close(Sock, Id) ->
    gen_sctp:eof(Sock, #sctp_assoc_change{assoc_id = Id}).
%% Having to pass a record here is hokey.

%% listener/2

listener(LRef, T) ->
    l(diameter_reg:match({?MODULE, listener, {LRef, '_'}}), LRef, T).

%% Existing process with the listening socket ...
l([{{?MODULE, listener, {_, AS}}, LPid}], _, _) ->
    {LAs, _Sock} = AS,
    {LPid, LAs};

%% ... or not: start one.
l([], LRef, T) ->
    {ok, LPid, LAs} = diameter_sctp_sup:start_child({listen, LRef, T}),
    {LPid, LAs}.

%% open/3

open(Addrs, Opts, PortNr) ->
    {LAs, Os} = addrs(Addrs, Opts),
    {LAs, case gen_sctp:open(gen_opts(portnr(Os, PortNr))) of
              {ok, Sock} ->
                  Sock;
              {error, Reason} ->
                  x({open, Reason})
          end}.

addrs(Addrs, Opts) ->
    case proplists:split(Opts, [ip]) of
        {[[]], _} ->
            {Addrs, Opts ++ [{ip, A} || A <- Addrs]};
        {[As], Os} ->
            LAs = [diameter_lib:ipaddr(A) || {ip, A} <- As],
            {LAs, Os ++ [{ip, A} || A <- LAs]}
    end.

portnr(Opts, PortNr) ->
    case proplists:get_value(port, Opts) of
        undefined ->
            [{port, PortNr} | Opts];
        _ ->
            Opts
    end.

%% x/1

x(Reason) ->
    exit({shutdown, Reason}).

%% gen_opts/1

gen_opts(Opts) ->
    {L,_} = proplists:split(Opts, [binary, list, mode, active, sctp_events]),
    [[],[],[],[],[]] == L orelse ?ERROR({reserved_options, Opts}),
    [binary, {active, once} | Opts].

%% ---------------------------------------------------------------------------
%% # handle_call/3
%% ---------------------------------------------------------------------------

handle_call({{accept, Ref}, Pid}, _, #listener{ref = Ref,
                                               count = N}
                                     = S) ->
    {TPid, NewS} = accept(Pid, S),
    {reply, {ok, TPid}, NewS#listener{count = N+1}};

handle_call(_, _, State) ->
    {reply, nok, State}.

%% ---------------------------------------------------------------------------
%% # handle_cast/2
%% ---------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%% ---------------------------------------------------------------------------
%% # handle_info/2
%% ---------------------------------------------------------------------------

handle_info(T, #transport{} = S) ->
    {noreply, #transport{} = t(T,S)};

handle_info(T, #listener{} = S) ->
    {noreply, #listener{} = l(T,S)}.

%% ---------------------------------------------------------------------------
%% # code_change/3
%% ---------------------------------------------------------------------------

code_change(_, State, _) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% # terminate/2
%% ---------------------------------------------------------------------------

terminate(_, #transport{assoc_id = undefined}) ->
    ok;

terminate(_, #transport{socket = Sock,
                        mode = {accept, _},
                        assoc_id = Id}) ->
    close(Sock, Id);

terminate(_, #transport{socket = Sock}) ->
    gen_sctp:close(Sock);

terminate(_, #listener{socket = Sock}) ->
    gen_sctp:close(Sock).

%% ---------------------------------------------------------------------------

%% start_timer/1

start_timer(#listener{count = 0} = S) ->
    S#listener{tref = erlang:start_timer(?LISTENER_TIMEOUT, self(), close)};
start_timer(S) ->
    S.

%% l/2
%%
%% Transition listener state.

%% Incoming message from SCTP.
l({sctp, Sock, _RA, _RP, Data} = Msg, #listener{socket = Sock} = S) ->
    setopts(Sock),
    case find(Data, S) of
        {TPid, NewS} ->
            TPid ! Msg,
            NewS;
        false ->
            S
    end;

%% Transport is asking message to be sent. See send/3 for why the send
%% isn't directly from the transport.
l({send, AssocId, StreamId, Bin}, #listener{socket = Sock} = S) ->
    send(Sock, AssocId, StreamId, Bin),
    S;

%% Accepting transport has died. One that's awaiting an association ...
l({'DOWN', MRef, process, TPid, _}, #listener{pending = [TPid | Q],
                                              tmap = T,
                                              count = N}
                                    = S) ->
    ets:delete(T, MRef),
    ets:delete(T, TPid),
    start_timer(S#listener{count = N-1,
                           pending = Q});

%% ... ditto and a new transport has already been started ...
l({'DOWN', _, process, _, _} = T, #listener{pending = [TPid | Q]}
                                  = S) ->
    #listener{pending = NQ}
        = NewS
        = l(T, S#listener{pending = Q}),
    NewS#listener{pending = [TPid | NQ]};

%% ... or not.
l({'DOWN', MRef, process, TPid, _}, #listener{socket = Sock,
                                              tmap = T,
                                              count = N,
                                              pending = {P,Q}}
                                    = S) ->
    [{MRef, Id}] = ets:lookup(T, MRef),  %% Id = TPid | AssocId
    ets:delete(T, MRef),
    ets:delete(T, Id),
    Id == TPid orelse close(Sock, Id),
    case ets:lookup(Q, TPid) of
        [{TPid, _}] -> %% transport in the pending queue ...
            ets:delete(Q, TPid),
            S#listener{pending = {P-1, Q}};
        [] ->           %% ... or not
            start_timer(S#listener{count = N-1})
    end;

%% Timeout after the last accepting process has died.
l({timeout, TRef, close = T}, #listener{tref = TRef,
                                        count = 0}) ->
    x(T);
l({timeout, _, close}, #listener{} = S) ->
    S.

%% t/2
%%
%% Transition transport state.

t(T,S) ->
    case transition(T,S) of
        ok ->
            S;
        #transport{} = NS ->
            NS;
        stop ->
            x(T)
    end.

%% transition/2

%% Incoming message.
transition({sctp, Sock, _RA, _RP, Data}, #transport{socket = Sock,
                                                    mode = {accept, _}}
                                         = S) ->
    recv(Data, S);

transition({sctp, Sock, _RA, _RP, Data}, #transport{socket = Sock} = S) ->
    setopts(Sock),
    recv(Data, S);

%% Outgoing message.
transition({diameter, {send, Msg}}, S) ->
    send(Msg, S);

%% Request to close the transport connection.
transition({diameter, {close, Pid}}, #transport{parent = Pid}) ->
    stop;

%% TLS over SCTP is described in RFC 3436 but has limitations as
%% described in RFC 6083. The latter describes DTLS over SCTP, which
%% addresses these limitations, DTLS itself being described in RFC
%% 4347. TLS is primarily used over TCP, which the current RFC 3588
%% draft acknowledges by equating TLS with TLS/TCP and DTLS/SCTP.
transition({diameter, {tls, _Ref, _Type, _Bool}}, _) ->
    stop;

%% Listener process has died.
transition({'DOWN', _, process, Pid, _}, #transport{mode = {accept, Pid}}) ->
    stop;

%% Parent process has died.
transition({'DOWN', _, process, Pid, _}, #transport{parent = Pid}) ->
    stop.

%% Crash on anything unexpected.

%% accept/2
%%
%% Start a new transport process or use one that's already been
%% started as a consequence of association establishment.

%% No pending associations: spawn a new transport.
accept(Pid, #listener{socket = Sock,
                      tmap = T,
                      pending = {0,_} = Q}
            = S) ->
    Arg = {accept, Pid, self(), Sock},
    {ok, TPid} = diameter_sctp_sup:start_child(Arg),
    MRef = erlang:monitor(process, TPid),
    ets:insert(T, [{MRef, TPid}, {TPid, MRef}]),
    {TPid, S#listener{pending = [TPid | Q]}};
%% Placing the transport in the pending field makes it available to
%% the next association. The stack starts a new accepting transport
%% only after this one brings the connection up (or dies).

%% Accepting transport has died. This can happen if a new transport is
%% started before the DOWN has arrived.
accept(Pid, #listener{pending = [TPid | {0,_} = Q]} = S) ->
    false = is_process_alive(TPid),  %% assert
    accept(Pid, S#listener{pending = Q});

%% Pending associations: attach to the first in the queue.
accept(Pid, #listener{ref = Ref, pending = {N,Q}} = S) ->
    TPid = ets:first(Q),
    TPid ! {Ref, Pid},
    ets:delete(Q, TPid),
    {TPid, S#listener{pending = {N-1, Q}}}.

%% send/2

%% Outbound Diameter message on a specified stream ...
send(#diameter_packet{bin = Bin, transport_data = {stream, SId}}, S) ->
    send(SId, Bin, S),
    S;

%% ... or not: rotate through all steams.
send(Bin, #transport{streams = {_, OS},
                     os = N}
          = S)
  when is_binary(Bin) ->
    send(N, Bin, S),
    S#transport{os = (N + 1) rem OS}.

%% send/3

%% Messages have to be sent from the controlling process, which is
%% probably a bug. Sending from here causes an inet_reply, Sock,
%% Status} message to be sent to the controlling process while
%% gen_sctp:send/4 here hangs.
send(StreamId, Bin, #transport{assoc_id = AId,
                               mode = {accept, LPid}}) ->
    LPid ! {send, AId, StreamId, Bin};

send(StreamId, Bin, #transport{socket = Sock,
                               assoc_id = AId}) ->
    send(Sock, AId, StreamId, Bin).

%% send/4

send(Sock, AssocId, Stream, Bin) ->
    case gen_sctp:send(Sock, AssocId, Stream, Bin) of
        ok ->
            ok;
        {error, Reason} ->
            x({send, Reason})
    end.

%% recv/2

%% Association established ...
recv({[], #sctp_assoc_change{state = comm_up,
                             outbound_streams = OS,
                             inbound_streams = IS,
                             assoc_id = Id}},
     #transport{assoc_id = undefined}
     = S) ->
    up(S#transport{assoc_id = Id,
                   streams = {IS, OS}});

%% ... or not: try the next address.
recv({[], #sctp_assoc_change{} = E},
     #transport{assoc_id = undefined,
                socket = Sock,
                mode = {connect = C, {[RA|RAs], RP, Es}}}
     = S) ->
    S#transport{mode = {C, connect(Sock, RAs, RP, [{RA,E} | Es])}};

%% Lost association after establishment.
recv({[], #sctp_assoc_change{}}, _) ->
    stop;

%% Inbound Diameter message.
recv({[#sctp_sndrcvinfo{stream = Id}], Bin}, #transport{parent = Pid})
  when is_binary(Bin) ->
    diameter_peer:recv(Pid, #diameter_packet{transport_data = {stream, Id},
                                             bin = Bin}),
    ok;

recv({[], #sctp_shutdown_event{assoc_id = Id}},
     #transport{assoc_id = Id}) ->
    stop;

%% Note that diameter_sctp(3) documents that sctp_events cannot be
%% specified in the list of options passed to gen_sctp and that
%% gen_opts/1 guards against this. This is to ensure that we know what
%% events to expect and also to ensure that we receive
%% #sctp_sndrcvinfo{} with each incoming message (data_io_event =
%% true). Adaptation layer events (ie. #sctp_adaptation_event{}) are
%% disabled by default so don't handle it. We could simply disable
%% events we don't react to but don't.

recv({[], #sctp_paddr_change{}}, _) ->
    ok;

recv({[], #sctp_pdapi_event{}}, _) ->
    ok.

%% up/1

up(#transport{parent = Pid,
              mode = {connect = C, {[RA | _], RP, _}}}
   = S) ->
    diameter_peer:up(Pid, {RA,RP}),
    S#transport{mode = C};

up(#transport{parent = Pid,
              mode = {accept, _}}
   = S) ->
    diameter_peer:up(Pid),
    S.

%% find/2

find({[#sctp_sndrcvinfo{assoc_id = Id}], _}
     = Data,
     #listener{tmap = T}
     = S) ->
    f(ets:lookup(T, Id), Data, S);

find({_, Rec} = Data, #listener{tmap = T} = S) ->
    f(ets:lookup(T, assoc_id(Rec)), Data, S).

%% New association and a transport waiting for one: use it.
f([],
  {_, #sctp_assoc_change{state = comm_up,
                         assoc_id = Id}},
  #listener{tmap = T,
            pending = [TPid | {_,_} = Q]}
  = S) ->
    [{TPid, MRef}] = ets:lookup(T, TPid),
    ets:insert(T, [{MRef, Id}, {Id, TPid}]),
    ets:delete(T, TPid),
    {TPid, S#listener{pending = Q}};

%% New association and no transport start yet: spawn one and place it
%% in the queue.
f([],
  {_, #sctp_assoc_change{state = comm_up,
                         assoc_id = Id}},
  #listener{ref = Ref,
            socket = Sock,
            tmap = T,
            pending = {N,Q}}
  = S) ->
    Arg = {accept, Ref, self(), Sock, Id},
    {ok, TPid} = diameter_sctp_sup:start_child(Arg),
    MRef = erlang:monitor(process, TPid),
    ets:insert(T, [{MRef, Id}, {Id, TPid}]),
    ets:insert(Q, {TPid, now()}),
    {TPid, S#listener{pending = {N+1, Q}}};

%% Known association ...
f([{_, TPid}], _, S) ->
    {TPid, S};

%% ... or not: discard.
f([], _, _) ->
    false.

%% assoc_id/1

assoc_id(#sctp_shutdown_event{assoc_id = Id}) ->
    Id;
assoc_id(#sctp_assoc_change{assoc_id = Id}) ->
    Id;
assoc_id(#sctp_sndrcvinfo{assoc_id = Id}) ->
    Id;
assoc_id(#sctp_paddr_change{assoc_id = Id}) ->
    Id;
assoc_id(#sctp_adaptation_event{assoc_id = Id}) ->
    Id.

%% connect/4

connect(_, [], _, Reasons) ->
    x({connect, lists:reverse(Reasons)});

connect(Sock, [Addr | AT] = As, Port, Reasons) ->
    case gen_sctp:connect_init(Sock, Addr, Port, []) of
        ok ->
            {As, Port, Reasons};
        {error, _} = E ->
            connect(Sock, AT, Port, [{Addr, E} | Reasons])
    end.

%% setopts/1

setopts(Sock) ->
    case inet:setopts(Sock, [{active, once}]) of
        ok -> ok;
        X  -> x({setopts, Sock, X})  %% possibly on peer disconnect
    end.
