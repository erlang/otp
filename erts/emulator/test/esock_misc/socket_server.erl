%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(socket_server).

-export([
         start/0, start/5,
         start_tcp/0,  start_tcp/1,  start_tcp/3,
         start_tcp4/0, start_tcp4/1, start_tcp4/2,
         start_tcp6/0, start_tcp6/1, start_tcp6/2,
         start_udp/0,  start_udp/1,  start_udp/3,
         start_udp4/0, start_udp4/1, start_udp4/2,
         start_udp6/0, start_udp6/1, start_udp6/2,
         start_sctp/0, start_sctp/1
        ]).

-define(LIB, socket_lib).

-record(manager,  {socket, msg, peek, acceptors, handler_id, handlers}).
-record(acceptor, {id, socket, manager, 
                   atimeout = 5000}).
-record(handler,  {socket, peek, msg, type, manager, 
                   stimeout = 5000, rtimeout = 5000}).

-define(NUM_ACCEPTORS, 5).

start() ->
    start_tcp().

start_tcp() ->
    start_tcp4().

start_tcp(Peek) ->
    start_tcp4(Peek).

start_tcp4() ->
    start_tcp4(false).

start_tcp4(Peek) ->
    start_tcp4(false, Peek).

start_tcp4(UseMsg, Peek) ->
    start_tcp(inet, UseMsg, Peek).

start_tcp6() ->
    start_tcp6(false).

start_tcp6(Peek) ->
    start_tcp6(false, Peek).

start_tcp6(UseMsg, Peek) ->
    start_tcp(inet6, UseMsg, Peek).

start_tcp(Domain, UseMsg, Peek) when is_boolean(UseMsg) andalso is_boolean(Peek) ->
    start(Domain, stream, tcp, UseMsg, Peek).

start_udp() ->
    start_udp4().

start_udp(Peek) ->
    start_udp4(Peek).

start_udp4() ->
    start_udp4(false).

start_udp4(Peek) ->
    start_udp4(false, Peek).

start_udp4(UseMsg, Peek) ->
    start_udp(inet, UseMsg, Peek).

start_udp6() ->
    start_udp6(false, false).

start_udp6(Peek) ->
    start_udp6(false, Peek).

start_udp6(UseMsg, Peek) ->
    start_udp(inet6, UseMsg, Peek).

start_udp(Domain, UseMsg, Peek) when is_boolean(UseMsg) andalso is_boolean(Peek) ->
    start(Domain, dgram, udp, UseMsg, Peek).


start_sctp() ->
    start_sctp(inet).

start_sctp(Domain) when ((Domain =:= inet) orelse (Domain =:= inet6)) ->
    start(Domain, seqpacket, sctp, true, false).

start(Domain, Type, Proto, UseMsg, Peek) ->
    put(sname, "starter"),
    i("try start manager"),
    {Pid, MRef} = manager_start(Domain, Type, Proto, UseMsg, Peek),
    i("manager (~p) started", [Pid]),
    loop(Pid, MRef).

loop(Pid, MRef) ->
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            i("manager process exited: "
              "~n   ~p", [Reason]),
            ok
    end.


%% =========================================================================

manager_start(Domain, Type, Proto, UseMsg, Peek) ->
    spawn_monitor(fun() -> manager_init(Domain, Type, Proto, UseMsg, Peek) end).

manager_start_handler(Pid, Sock) ->
    manager_request(Pid, {start_handler, Sock}).

manager_stop(Pid, Reason) ->
    manager_request(Pid, {stop, Reason}).

manager_request(Pid, Request) ->
    ?LIB:request(manager, Pid, Request).

manager_reply(Pid, Ref, Reply) ->
    ?LIB:reply(manager, Pid, Ref, Reply).


manager_init(Domain, Type, Proto, UseMsg, Peek) ->
    put(sname, "manager"),
    do_manager_init(Domain, Type, Proto, UseMsg, Peek).

do_manager_init(Domain, stream = Type, Proto, UseMsg, Peek) ->
    i("try start acceptor(s)"),
    {Sock, Acceptors} = manager_stream_init(Domain, Type, Proto),
    manager_loop(#manager{socket     = Sock,
                          msg        = UseMsg,
                          peek       = Peek,
                          acceptors  = Acceptors,
                          handler_id = 1,
                          handlers   = []});
do_manager_init(Domain, dgram = Type, Proto, UseMsg, Peek) ->
    i("try open socket"),
    case socket:open(Domain, Type, Proto) of
        {ok, Sock} ->
            F = fun(X) -> case socket:getopt(Sock, socket, X) of
                              {ok, V} ->    f("~p", [V]);
                              {error, R} -> f("error: ~p", [R])
                          end
                end,
            i("socket opened (~s,~s,~s): "
              "~n   broadcast: ~s"
              "~n   dontroute: ~s"
              "~n   keepalive: ~s"
              "~n   reuseaddr: ~s"
              "~n   linger:    ~s"
              "~n   debug:     ~s"
              "~n   prio:      ~s"
              "~n   rcvbuf:    ~s"
              "~n   rcvtimeo:  ~s"
              "~n   sndbuf:    ~s"
              "~n   sndtimeo:  ~s"
              "~n   => try find (local) address", 
              [F(domain), F(type), F(protocol), 
               F(broadcast), F(dontroute), F(keepalive), F(reuseaddr), F(linger),
               F(debug), F(priority), 
               F(rcvbuf), F(rcvtimeo), F(sndbuf), F(sndtimeo)]),
            Addr = which_addr(Domain),
            SA = #{family => Domain,
                   addr   => Addr},
            i("try bind to: "
              "~n   ~p", [Addr]),
            case socket:bind(Sock, SA) of
                {ok, _P} ->
                   ok;
                {error, BReason} ->
                    throw({bind, BReason})
            end,
            i("bound to: "
              "~n   ~s"
              "~n   => try start handler", 
              [case socket:sockname(Sock) of
                   {ok, Name} -> f("~p", [Name]);
                   {error, R} -> f("error: ~p", [R])
               end]),
            case handler_start(1, Sock, UseMsg, Peek) of
                {ok, {Pid, MRef}} ->
                    i("handler (~p) started", [Pid]),
                    handler_continue(Pid),
                    manager_loop(#manager{peek       = Peek,
                                          msg        = UseMsg,
                                          handler_id = 2, % Just in case
                                          handlers   = [{1, Pid, MRef}]});
                {error, SReason} ->
                    e("Failed starting handler: "
                      "~n   ~p", [SReason]),
                    exit({failed_start_handler, SReason})
            end;
        {error, OReason} ->
            e("Failed open socket: "
              "~n   ~p", [OReason]),
            exit({failed_open_socket, OReason})
    end;
do_manager_init(Domain, seqpacket = Type, sctp = Proto, _UseMsg, _Peek) ->
    %% This is as far as I have got with SCTP at the moment...
    case socket:open(Domain, Type, Proto) of
        {ok, Sock} ->
            i("(sctp) socket opened: "
              "~n   ~p", [Sock]),
            EXP = fun(_Desc, Expect, Expect) ->
                          Expect;
                     (Desc, Expect, Actual) ->
                          e("Unexpected result ~w: "
                            "~n   Expect: ~p"
                            "~n   Actual: ~p", [Desc, Expect, Actual]),
                          exit({Desc, Expect, Actual})
                  end,
            GO = fun(O) -> case socket:getopt(Sock, sctp, O) of
                               {ok, V}    -> f("~p", [V]);
                               {error, R} -> f("error: ~p", [R])
                           end
                 end,
            %% ok = socket:setopt(Sock, otp, debug, true),

            i("Miscellaneous options: "
              "~n   associnfo:         ~s"
              "~n   autoclose:         ~s"
              "~n   disable-fragments: ~s"
              "~n   initmsg:           ~s"
              "~n   maxseg:            ~s"
              "~n   nodelay:           ~s"
              "~n   rtoinfo:           ~s", 
              [GO(associnfo),
               GO(autoclose),
               GO(disable_fragments),
               GO(initmsg),
               GO(maxseg),
               GO(nodelay),
               GO(rtoinfo)]),

            Events = #{data_in          => true,
                       association      => true,
                       address          => true,
                       send_failure     => true,
                       peer_error       => true,
                       shutdown         => true,
                       partial_delivery => true,
                       adaptation_layer => true,
                       authentication   => true,
                       sender_dry       => true},
            EXP(set_sctp_events, ok, socket:setopt(Sock, sctp, events, Events)),
            EXP(close_socket, ok, socket:close(Sock));
        {error, Reason} ->
            exit({failed_open, Reason})
    end;
do_manager_init(Domain, raw = Type, Proto, UseMsg, Peek) when is_integer(Proto) ->
    do_manager_init(Domain, Type, {raw, Proto}, UseMsg, Peek);
do_manager_init(Domain, raw = Type, Proto, _UseMsg, _Peek) ->
    case socket:open(Domain, Type, Proto) of
        {ok, Sock} ->
            i("(sctp) socket opened: "
              "~n   ~p", [Sock]),
            socket:close(Sock);
        {error, Reason} ->
            exit({failed_open, Reason})
    end.



manager_stream_init(Domain, Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    F = fun(X) -> case socket:getopt(Sock, socket, X) of
                      {ok, V} ->    f("~p", [V]);
                      {error, R} -> f("error: ~p", [R])
                  end
        end,
    i("(socket) open (~s,~s,~s): "
      "~n   debug:     ~s"
      "~n   prio:      ~s"
      "~n   => try find (local) address", 
      [F(domain), F(type), F(protocol), F(debug), F(priority)]),
    Addr = which_addr(Domain),
    SA = #{family => Domain,
           addr   => Addr},
    i("found: "
      "~n   ~p"
      "~n   => try (socket) bind", [Addr]),
    %% ok = socket:setopt(Sock, otp, debug, true),
    %% ok = socket:setopt(Sock, socket, debug, 1), %% must have rights!!
    Port = case socket:bind(Sock, SA) of
               {ok, P} ->
                   %% ok = socket:setopt(Sock, socket, debug, 0), %% must have rights!!
                   %% ok = socket:setopt(Sock, otp, debug, false),
                   P;
               {error, BReason} ->
                   throw({bind, BReason})
           end,
    i("bound to: "
      "~n   ~p"
      "~n   => try (socket) listen (acceptconn: ~s)", 
      [Port, F(acceptconn)]),
    case socket:listen(Sock) of
        ok ->
            i("listening (acceptconn: ~s)", 
              [F(acceptconn)]),
            manager_stream_init(Sock, 1, ?NUM_ACCEPTORS, []);
        {error, LReason} ->
            throw({listen, LReason})
    end.

which_addr(Domain) ->
    Iflist = case inet:getifaddrs() of
                 {ok, IFL} ->
                     IFL;
                 {error, Reason} ->
                     throw({inet,getifaddrs,Reason})
             end,
    which_addr(Domain, Iflist).

which_addr(_Domain, []) ->
    throw(no_address);
which_addr(Domain, [{Name, IFO}|_IFL]) when (Name =/= "lo") ->
    which_addr2(Domain, IFO);
which_addr(Domain, [_|IFL]) ->
    which_addr(Domain, IFL).

which_addr2(_, []) ->
    throw(no_address);
which_addr2(inet = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 4) ->
    Addr;
which_addr2(inet6 = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 8) ->
    Addr;
which_addr2(Domain, [_|IFO]) ->
    which_addr2(Domain, IFO).


manager_stream_init(Sock, ID, NumAcceptors, Acc) 
  when (NumAcceptors > 0) ->
    i("try start acceptor"),
    case acceptor_start(Sock, ID) of
        {ok, {Pid, MRef}} ->
            i("acceptor ~w (~p) started", [ID, Pid]),
            ?LIB:sleep(2000),
            manager_stream_init(Sock, ID+1, NumAcceptors-1, 
                                [{ID, Pid, MRef}|Acc]);
        {error, Reason} ->
            exit({failed_starting_acceptor, Reason})
    end;
manager_stream_init(Sock, _ID, 0, Acc) ->
    %% Req = {kill_acceptor, length(Acc)}, % Last in the queue
    %% Req = {kill_acceptor, 3},           % In the "middle" of the queue
    %% Req = {kill_acceptor, 2},           % The first in the queue
    %% Req = {kill_acceptor, 1},           % Current acceptor
    %% Msg = {manager, self(), make_ref(), Req},
    %% erlang:send_after(timer:seconds(10), self(), Msg),
    {Sock, lists:reverse(Acc)}.


manager_loop(M) ->
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            M2 = manager_handle_down(M, MRef, Pid, Reason),
            manager_loop(M2);
        
        {manager, Pid, Ref, Request} ->
            M2 = manager_handle_request(M, Pid, Ref, Request),
            manager_loop(M2)
    end.


manager_handle_down(#manager{acceptors = Acceptors,
                             handlers  = Handlers} = M, MRef, Pid, Reason) ->
    case lists:keysearch(Pid, 2, Acceptors) of
        {value, {ID, Pid, MRef}} when (Reason =:= normal) ->
            i("acceptor ~w exited (normally)", [ID]),
            case lists:keydelete(Pid, 2, Acceptors) of
                [] ->
                    %% We are done
                    i("the last acceptor - we are done"),
                    exit(normal);
                Acceptors2 ->
                    M#manager{acceptors = Acceptors2}
            end;
        {value, {ID, Pid, MRef}} ->
            e("acceptor ~w crashed: "
              "~n   ~p", [ID, Reason]),
            exit({acceptor_died, Reason});

        false -> %% handler!
            if
                (Reason =/= normal) ->
                    e("handler ~p died: "
                      "~n   ~p", [Pid, Reason]);
                true ->
                    i("handler ~p terminated", [Pid])
            end,
            Handlers2 = lists:keydelete(Pid, 2, Handlers),
            M#manager{handlers = Handlers2}
    end.


manager_handle_request(#manager{peek       = Peek,
                                msg        = UseMsg,
                                handler_id = HID,
                                handlers   = Handlers} = M, Pid, Ref,
                       {start_handler, Sock}) ->
    i("try start handler (~w)", [HID]),
    case handler_start(HID, Sock, UseMsg, Peek) of
        {ok, {HPid, HMRef}} ->
            i("handler ~w started", [HID]),
            manager_reply(Pid, Ref, {ok, HPid}),
            M#manager{handler_id = HID+1,
                      handlers   = [{HID, HPid, HMRef}|Handlers]};
        {error, Reason} = ERROR ->
            e("Failed starting new handler: "
              "~n   Sock:   ~p"
              "~n   Reason: ~p", [Sock, Reason]),
            manager_reply(Pid, Ref, ERROR),
            M
    end;
manager_handle_request(#manager{socket    = Sock,
                                acceptors = [{AID, APid, AMRef}]} = M, _Pid, _Ref,
                       {kill_acceptor, AID}) ->
    i("try kill (only remeining) acceptor ~w", [AID]),
    socket:setopt(Sock, otp, debug, true),
    manager_stop_acceptor(APid, AMRef, AID, kill),
    M#manager{acceptors = []};
manager_handle_request(#manager{socket    = Sock,
                                acceptors = Acceptors} = M, _Pid, _Ref,
                       {kill_acceptor, AID}) ->
    i("try kill acceptor ~w", [AID]),
    case lists:keysearch(AID, 1, Acceptors) of
        {value, {AID, APid, AMRef}} ->
            socket:setopt(Sock, otp, debug, true),
            manager_stop_acceptor(APid, AMRef, AID, kill),
            Acceptors2 = lists:keydelete(AID, 1, Acceptors),
            M#manager{acceptors = Acceptors2};
        false ->
            e("no such acceptor"),
            M
    end;
manager_handle_request(#manager{acceptors = Acceptors,
                                handlers  = Handlers}, Pid, Ref,
                       {stop, Reason}) ->
    i("stop"),
    manager_reply(Pid, Ref, ok),
    manager_stop_handlers(Handlers, Reason),
    manager_stop_acceptors(Acceptors, Reason),
    i("stopped", []),
    exit(Reason).

manager_stop_acceptors(Acceptors, Reason) ->
    lists:foreach(fun({ID,P,M}) -> 
                          manager_stop_acceptor(P, M, ID, Reason) 
                  end, Acceptors).
    
manager_stop_acceptor(Pid, MRef, ID, Reason) ->
    i("try stop acceptor ~w (~p): ~p", [ID, Pid, Reason]),
    erlang:demonitor(MRef, [flush]),
    acceptor_stop(Pid, Reason),
    ok.

manager_stop_handlers(Handlers, Reason) ->
    lists:foreach(fun({ID,P,M}) -> 
                          manager_stop_handler(P, M, ID, Reason) 
                  end, Handlers).

manager_stop_handler(Pid, MRef, ID, Reason) ->
    i("try stop handler ~w (~p): ~p", [ID, Pid, Reason]),
    erlang:demonitor(MRef, [flush]),
    handler_stop(Pid, Reason),
    ok.

    

%% =========================================================================

acceptor_start(Sock, ID) ->
    Self = self(),
    A = {Pid, _} = spawn_monitor(fun() -> 
                                         acceptor_init(Self, Sock, ID) 
                                 end),
    receive
        {acceptor, Pid, ok} ->
            {ok, A};
        {acceptor, Pid, {error, _} = Error} ->
            exit(Pid, kill), % Just in case
            Error;
        {'DOWN', _MRef, process, Pid, Reason} ->
            {error, {crashed, Reason}}
    end.

acceptor_stop(Pid, _Reason) ->
    %% acceptor_request(Pid, {stop, Reason}).
    exit(Pid, kill).

%% acceptor_request(Pid, Request) ->
%%     request(acceptor, Pid, Request).

%% acceptor_reply(Pid, Ref, Reply) ->
%%     reply(acceptor, Pid, Ref, Reply).


acceptor_init(Manager, Sock, ID) ->
    put(sname, f("acceptor[~w]", [ID])),
    Manager ! {acceptor, self(), ok},
    %% ok = socket:setopt(Sock, otp, debug, true),
    acceptor_loop(#acceptor{id      = ID,
                            manager = Manager,
                            socket  = Sock}).

acceptor_loop(#acceptor{socket = LSock, atimeout = Timeout} = A) ->
    i("try accept"),
    case socket:accept(LSock, Timeout) of
        {ok, Sock} ->
            i("accepted: "
              "~n   ~p"
              "~nwhen"
              "~n   ~p", [Sock, socket:info()]),
            case acceptor_handle_accept_success(A, Sock) of
                ok ->
                    acceptor_loop(A);
                {error, Reason} ->
                    e("Failed starting handler: "
                      "~n   ~p", [Reason]),
                    socket:close(Sock),
                    exit({failed_starting_handler, Reason})
            end;
        {error, timeout} ->
            i("timeout"),
            acceptor_loop(A);
        {error, Reason} ->
            e("accept failure: "
              "~n   ~p", [Reason]),
            exit({accept, Reason})
    end.

acceptor_handle_accept_success(#acceptor{manager = Manager}, Sock) ->
    i("try start handler for peer"
      "~n   ~p", [case socket:peername(Sock) of
                      {ok, Peer} -> Peer;
                      {error, _} = E -> E
                  end]),
    case manager_start_handler(Manager, Sock) of
        {ok, Pid} ->
            i("handler (~p) started - now change 'ownership'", [Pid]),
            case socket:setopt(Sock, otp, controlling_process, Pid) of
                ok ->
                    %% Normally we should have a msgs collection here
                    %% (of messages we receive before the control was
                    %% handled over to Handler), but since we don't 
                    %% have active implemented yet...
                    i("new handler (~p) now controlling process", [Pid]),
                    handler_continue(Pid),
                    ok;
                {error, _} = ERROR ->
                    exit(Pid, kill),
                    ERROR
            end;
        {error, Reason2} ->
            e("failed starting handler: "
              "~n   (new) Socket: ~p"
              "~n   Reason:       ~p", [Sock, Reason2]),
            exit({failed_starting_handler, Reason2})
    end.



%% =========================================================================

handler_start(ID, Sock, UseMsg, Peek) ->
    Self = self(),
    H = {Pid, _} = spawn_monitor(fun() ->
                                         handler_init(Self, ID, UseMsg, Peek, Sock)
                                 end),
    receive
        {handler, Pid, ok} ->
            {ok, H};
        {handler, Pid, {error, _} = ERROR} ->
            exit(Pid, kill), % Just in case
            ERROR
    end.

handler_stop(Pid, _Reason) ->
    %% handler_request(Pid, {stop, Reason}).
    exit(Pid, kill).

handler_continue(Pid) ->
    handler_request(Pid, continue).

handler_request(Pid, Request) ->
    ?LIB:request(handler, Pid, Request).

handler_reply(Pid, Ref, Reply) ->
    ?LIB:reply(handler, Pid, Ref, Reply).


handler_init(Manager, ID, Msg, Peek, Sock) ->
    put(sname, f("handler:~w", [ID])),
    i("starting"),
    Manager ! {handler, self(), ok},
    receive
        {handler, Pid, Ref, continue} ->
            i("got continue"),
            handler_reply(Pid, Ref, ok),
            G = fun(L, O) -> case socket:getopt(Sock, L, O) of
                                {ok, Val} ->
                                    f("~p", [Val]);
                                {error, R} when is_atom(R) ->
                                    f("error: ~w", [R]);
                                {error, {T, R}} when is_atom(T) ->
                                    f("error: ~w, ~p", [T, R]);
                                {error, R} ->
                                    f("error: ~p", [R])
                          end
                end,
            GSO  = fun(O) -> G(socket, O) end,
            GIP4 = fun(O) -> G(ip,     O) end,
            GIP6 = fun(O) -> G(ipv6,   O) end,
            {ok, Domain} = socket:getopt(Sock, socket, domain),
            {ok, Type}   = socket:getopt(Sock, socket, type),
            {ok, Proto}  = socket:getopt(Sock, socket, protocol),
            B2D          = GSO(bindtodevice),
            RA           = GSO(reuseaddr),
            RP           = GSO(reuseport),
            OOBI         = GSO(oobinline),
            RcvBuf       = GSO(rcvbuf),
            RcvLW        = GSO(rcvlowat),
            RcvTO        = GSO(rcvtimeo),
            SndBuf       = GSO(sndbuf),
            SndLW        = GSO(sndlowat),
            SndTO        = GSO(sndtimeo),
            Linger       = GSO(linger),
            Timestamp    = GSO(timestamp),
            FreeBind     = GIP4(freebind),
            MTU          = GIP4(mtu),
            MTUDisc      = GIP4(mtu_discover),
            MALL         = GIP4(multicast_all),
            MIF4         = GIP4(multicast_if),
            MLoop4       = GIP4(multicast_loop),
            MTTL         = GIP4(multicast_ttl),
            NF           = GIP4(nodefrag), % raw only
            PktInfo      = GIP4(pktinfo),  % dgram only
            RecvErr4     = GIP4(recverr),
            RecvIF       = GIP4(recvif),   % Only dgram and raw (and FreeBSD)
            RecvOPTS     = GIP4(recvopts), % Not stream
            RecvOrigDstAddr = GIP4(recvorigdstaddr),
            RecvTOS      = GIP4(recvtos),
            RecvTTL      = GIP4(recvttl),  % not stream
            RetOpts      = GIP4(retopts),  % not stream
            SendSrcAddr  = GIP4(sendsrcaddr),
            TOS          = GIP4(tos),
            Transparent  = GIP4(transparent),
            TTL          = GIP4(ttl),
            MHops        = GIP6(multicast_hops),
            MIF6         = GIP6(multicast_if), % Only dgram and raw
            MLoop6       = GIP6(multicast_loop),
            RecvErr6     = GIP6(recverr),
            RecvPktInfo  = GIP6(recvpktinfo),
            RtHdr        = GIP6(rthdr),
            AuthHdr      = GIP6(authhdr),
            HopLimit     = GIP6(hoplimit),
            HopOpts      = GIP6(hopopts),
            DstOpts      = GIP6(dstopts),
            FlowInfo     = GIP6(flowinfo),
            UHops        = GIP6(unicast_hops),
            i("got continue when: "
              "~n   (socket) Domain:             ~p"
              "~n   (socket) Type:               ~p"
              "~n   (socket) Protocol:           ~p"
              "~n   (socket) Reuse Address:      ~s"
              "~n   (socket) Reuse Port:         ~s"
              "~n   (socket) Bind To Device:     ~s"
              "~n   (socket) OOBInline:          ~s"
              "~n   (socket) RcvBuf:             ~s"
              "~n   (socket) RcvLW:              ~s"
              "~n   (socket) RcvTO:              ~s"
              "~n   (socket) SndBuf:             ~s"
              "~n   (socket) SndLW:              ~s"
              "~n   (socket) SndTO:              ~s"
              "~n   (socket) Linger:             ~s"
              "~n   (socket) Timestamp:          ~s"
              "~n   (ip)     FreeBind:           ~s"
              "~n   (ip)     MTU:                ~s"
              "~n   (ip)     MTU Discovery:      ~s"
              "~n   (ip)     Multicast ALL:      ~s"
              "~n   (ip)     Multicast IF:       ~s"
              "~n   (ip)     Multicast Loop:     ~s"
              "~n   (ip)     Multicast TTL:      ~s"
              "~n   (ip)     Node Frag:          ~s"
              "~n   (ip)     Pkt Info:           ~s"
              "~n   (ip)     Recv Err:           ~s"
              "~n   (ip)     Recv IF:            ~s"
              "~n   (ip)     Recv OPTS:          ~s"
              "~n   (ip)     Recv Orig Dst Addr: ~s"
              "~n   (ip)     Recv TOS:           ~s"
              "~n   (ip)     Recv TTL:           ~s"
              "~n   (ip)     Ret Opts:           ~s"
              "~n   (ip)     Send Src Addr:      ~s"
              "~n   (ip)     TOS:                ~s"
              "~n   (ip)     Transparent:        ~s"
              "~n   (ip)     TTL:                ~s"
              "~n   (ipv6)   Multicast Hops:     ~s"
              "~n   (ipv6)   Multicast IF:       ~s"
              "~n   (ipv6)   Multicast Loop:     ~s"
              "~n   (ipv6)   Recv Err:           ~s"
              "~n   (ipv6)   Recv Pkt Info:      ~s"
              "~n   (ipv6)   RT Hdr:             ~s"
              "~n   (ipv6)   Auth Hdr:           ~s"
              "~n   (ipv6)   Hop Limit:          ~s"
              "~n   (ipv6)   Hop Opts:           ~s"
              "~n   (ipv6)   Dst Opts:           ~s"
              "~n   (ipv6)   Flow Info:          ~s"
              "~n   (ipv6)   Unicast Hops:       ~s",
              [Domain, Type, Proto,
               RA, RP, B2D, OOBI,
               RcvBuf, RcvLW, RcvTO, SndBuf, SndLW, SndTO,
               Linger, Timestamp,
               FreeBind, MTU, MTUDisc, MALL, MIF4, MLoop4, MTTL,
               NF, PktInfo,RecvErr4, 
               RecvIF, RecvOPTS, RecvOrigDstAddr, RecvTOS, RecvTTL, RetOpts,
               SendSrcAddr, TOS, Transparent, TTL,
               MHops, MIF6, MLoop6, RecvErr6, RecvPktInfo,
               RtHdr, AuthHdr, HopLimit, HopOpts, DstOpts, FlowInfo,
               UHops]),

            %% ok = socket:setopt(Sock, otp, debug, true),
            %% case socket:getopt(Sock, 0, {13, int}) of
            %%     {ok, Val} ->
            %%         i("PktOpts ok:  ~p", [Val]);
            %%     {error, Reason} ->
            %%         e("PktOpts err: ~p", [Reason])
            %% end,
            %% ok = socket:setopt(Sock, otp, debug, false),
            SSO = fun(O, V) -> soso(Sock, O, V) end,
            SIP4 = 
                fun(O, V) ->
                        if 
                            (Type =:= dgram) -> 
                                ok = soip(Sock, O,  V);
                            true ->
                                ok
                        end
                end,
            SSO(timestamp, true),
            SIP4(pktinfo, true),
            ok = soip(Sock, recvtos,  true),
            SIP4(recvttl,  true),
            ok = soip(Sock, recvorigdstaddr,  true),

            handler_loop(#handler{msg     = Msg,
                                  peek    = Peek,
                                  manager = Manager,
                                  type    = Type,
                                  socket  = Sock})
    end.

so(Sock, Lvl, Opt, Val) ->
    ok = socket:setopt(Sock, Lvl, Opt, Val).

soso(Sock, Opt, Val) ->
    so(Sock, socket, Opt, Val).

soip(Sock, Opt, Val) ->
    so(Sock, ip, Opt, Val).

%% soipv6(Sock, Opt, Val) ->
%%     so(Sock, ipv6, Opt, Val).

handler_loop(H) ->
    i("try read message"),
    case recv(H) of
        {ok, {Source, Msg}} ->
            i("received ~w bytes of data~s", 
              [size(Msg), case Source of
                              undefined -> "";
                              _ -> f(" from:~n   ~p", [Source])
                          end]),
            case ?LIB:dec_msg(Msg) of
                {request, N, Req} ->
                    i("received request ~w: "
                      "~n   ~p", [N, Req]),
                    Reply = ?LIB:enc_rep_msg(N, "hoppsan"),
                    case send(H, Reply, Source) of
                        ok ->
                            i("successfully sent reply ~w", [N]),
                            handler_loop(H);
                        {error, SReason} ->
                            e("failed sending reply ~w:"
                              "~n   ~p", [N, SReason]),
                            exit({failed_sending_reply, SReason})
                    end
            end;

        {error, closed} ->
            i("closed when"
              "~n   ~p", [socket:info()]),
            exit(normal);
        
        {error, RReason} ->
            e("failed reading request: "
              "~n   ~p", [RReason]),
            exit({failed_reading_request, RReason})
    end.


recv(#handler{peek = true, socket = Sock, type = stream}) ->
    peek_recv(Sock);
recv(#handler{socket = Sock, msg = true, type = stream}) ->
    case socket:recvmsg(Sock) of
        {ok, #{addr  := undefined = Source,
               iov   := [Data],
               ctrl  := CMsgHdrs,
               flags := Flags}} ->
            i("received message: "
              "~n   CMsgHdrs: ~p"
              "~n   Flags:    ~p", [CMsgHdrs, Flags]),
            {ok, {Source, Data}};
        {ok, X} ->
            e("received *unexpected* message: "
              "~n   ~p", [X]),
            {error, {unexpected, X}};
        {error, _} = ERROR ->
            ERROR
    end;
recv(#handler{socket = Sock, msg = true, type = dgram}) ->
    case socket:recvmsg(Sock) of
        {ok, #{addr  := Source,
               iov   := [Data],
               ctrl  := CMsgHdrs,
               flags := Flags}} ->
            i("received message: "
              "~n   CMsgHdrs: ~p"
              "~n   Flags:    ~p", [CMsgHdrs, Flags]),
            {ok, {Source, Data}};
        {ok, X} ->
            {error, {unexpected, X}};
        {error, _} = ERROR ->
            ERROR
    end;
recv(#handler{peek = false, socket = Sock, type = stream}) ->
    do_recv(Sock);
recv(#handler{peek = Peek, socket = Sock, type = dgram})
  when (Peek =:= true) ->
    %% ok  = socket:setopt(Sock, otp, debug, true),
    RES = peek_recvfrom(Sock, 5),
    %% ok  = socket:setopt(Sock, otp, debug, false),
    RES;
recv(#handler{peek = Peek, socket = Sock, type = dgram})
  when (Peek =:= false) ->
    %% ok = socket:setopt(Sock, otp, debug, true),
    socket:recvfrom(Sock).

do_recv(Sock) ->
    case socket:recv(Sock) of
        {ok, Msg} ->
            {ok, {undefined, Msg}};
        {error, _} = ERROR ->
            ERROR
    end.

peek_recv(Sock) ->
    i("try peek on the message type (expect request)"),
    Type = ?LIB:req(),
    case socket:recv(Sock, 4, [peek]) of
        {ok, <<Type:32>>} ->
            i("was request - do proper recv"),
            do_recv(Sock);
        {error, _} = ERROR ->
            ERROR
    end.

peek_recvfrom(Sock, BufSz) ->
    i("try peek recvfrom with buffer size ~w", [BufSz]),
    case socket:recvfrom(Sock, BufSz, [peek]) of
        {ok, {_Source, Msg}} when (BufSz =:= size(Msg)) ->
            %% i("we filled the buffer: "
            %%   "~n   ~p", [Msg]),
            %% It *may not* fit => try again with double size
            peek_recvfrom(Sock, BufSz*2);
        {ok, _} ->
            %% It fits => read for real
            i("we did *not* fill the buffer - do the 'real' read"),
            socket:recvfrom(Sock);
        {error, _} = ERROR ->
            ERROR
    end.


send(#handler{socket = Sock, msg = true, type = stream, stimeout = Timeout}, 
     Msg, _) ->
    CMsgHdr  = #{level => ip, type => tos, data => reliability},
    CMsgHdrs = [CMsgHdr],
    MsgHdr   = #{iov => [Msg], ctrl => CMsgHdrs},
    %% socket:setopt(Sock, otp, debug, true),
    Res = socket:sendmsg(Sock, MsgHdr, Timeout),
    %% socket:setopt(Sock, otp, debug, false),
    Res;
send(#handler{socket = Sock, type = stream, stimeout = Timeout}, Msg, _) ->
    socket:send(Sock, Msg, Timeout);
send(#handler{socket = Sock, msg = true, type = dgram, stimeout = Timeout}, 
     Msg, Dest) ->
    CMsgHdr  = #{level => ip, type => tos, data => reliability},
    CMsgHdrs = [CMsgHdr],
    MsgHdr   = #{addr => Dest,
                 ctrl => CMsgHdrs,
                 iov  => [Msg]},
    %% ok = socket:setopt(Sock, otp, debug, true),
    Res = socket:sendmsg(Sock, MsgHdr, Timeout),
    %% ok = socket:setopt(Sock, otp, debug, false),
    Res;
send(#handler{socket = Sock, type = dgram, stimeout = Timeout}, Msg, Dest) ->
    socket:sendto(Sock, Msg, Dest, Timeout).

%% filler() ->
%%     list_to_binary(lists:duplicate(2048, " FILLER ")).



%% =========================================================================

f(F, A) ->
    ?LIB:f(F, A).

e(F) ->
    e(F, []).
e(F, A) ->
    ?LIB:e(F, A).

i(F) ->
    ?LIB:i(F).

i(F, A) ->
    ?LIB:i(F, A).

