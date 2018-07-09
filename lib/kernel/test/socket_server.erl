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
         start/0,
         start_tcp/0, start_tcp/1,
         start_udp/0, start_udp/1
        ]).

-define(LIB, socket_lib).

-record(manager,  {peek, acceptor, handler_id, handlers}).
-record(acceptor, {socket, manager}).
-record(handler,  {socket, peek, type, manager}).

start() ->
    start_tcp().

start_tcp() ->
    start_tcp(false).

start_tcp(Peek) ->
    start(inet, stream, tcp, Peek).

start_udp() ->
    start_udp(false).

start_udp(Peek) when is_boolean(Peek) ->
    start(inet, dgram, udp, Peek).

start(Domain, Type, Proto, Peek) ->
    put(sname, "starter"),
    i("try start manager"),
    {Pid, MRef} = manager_start(Domain, Type, Proto, Peek),
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

manager_start(Domain, Type, Proto, Peek) ->
    spawn_monitor(fun() -> manager_init(Domain, Type, Proto, Peek) end).

manager_start_handler(Pid, Sock) ->
    manager_request(Pid, {start_handler, Sock}).

manager_stop(Pid, Reason) ->
    manager_request(Pid, {stop, Reason}).

manager_request(Pid, Request) ->
    ?LIB:request(manager, Pid, Request).

manager_reply(Pid, Ref, Reply) ->
    ?LIB:reply(manager, Pid, Ref, Reply).


manager_init(Domain, stream = Type, Proto, Peek) ->
    put(sname, "manager"),
    i("try start acceptor"),
    case acceptor_start(Domain, Type, Proto) of
        {ok, {Pid, MRef}} ->
            i("acceptor started"),
            manager_loop(#manager{peek       = Peek,
                                  acceptor   = {Pid, MRef},
                                  handler_id = 1,
                                  handlers   = []});
        {error, Reason} ->
            exit({failed_starting_acceptor, Reason})
    end;
manager_init(Domain, dgram = Type, Proto, Peek) ->
    put(sname, "manager"),
    i("try open socket"),
    case socket:open(Domain, Type, Proto) of
        {ok, Sock} ->
            Addr = which_addr(Domain),
            SA = #{family => Domain,
                   addr   => Addr},
            case socket:bind(Sock, SA) of
                {ok, _P} ->
                   ok;
                {error, BReason} ->
                    throw({bind, BReason})
            end,
            i("try start handler for"
              "~n   ~p", [case socket:sockname(Sock) of
                              {ok, Name} -> Name;
                              {error, _} = E -> E
                          end]),
            case handler_start(1, Sock, Peek) of
                {ok, {Pid, MRef}} ->
                    i("handler (~p) started", [Pid]),
                    handler_continue(Pid),
                    manager_loop(#manager{peek       = Peek,
                                          handler_id = 2, % Just in case
                                          handlers   = [{Pid, MRef, 1}]});
                {error, SReason} ->
                    e("Failed starting handler: "
                      "~n   ~p", [SReason]),
                    exit({failed_start_handler, SReason})
            end;
        {error, OReason} ->
            e("Failed open socket: "
              "~n   ~p", [OReason]),
            exit({failed_open_socket, OReason})
    end.


manager_loop(M) ->
    receive
        {'DOWN', MRef, process, Pid, Reason} ->
            M2 = manager_handle_down(M, MRef, Pid, Reason),
            manager_loop(M2);

        {manager, Pid, Ref, Request} ->
            M2 = manager_handle_request(M, Pid, Ref, Request),
            manager_loop(M2)
    end.


manager_handle_down(#manager{acceptor = {Pid, MRef}}, MRef, Pid, Reason) 
  when (Reason =/= normal) ->
    e("acceptor died: "
      "~n   ~p", [Reason]),
    exit({acceptor_died, Reason});
manager_handle_down(#manager{acceptor = {Pid, MRef}}, MRef, Pid, Reason) ->
    exit(Reason);
manager_handle_down(#manager{handlers = Handlers} = M, _MRef, Pid, Reason) ->
    if
        (Reason =/= normal) ->
            e("handler ~p died: "
              "~n   ~p", [Pid, Reason]);
        true ->
            i("handler ~p terminated", [Pid])
    end,
    Handlers2 = lists:keydelete(Pid, 1, Handlers),
    M#manager{handlers = Handlers2}.


manager_handle_request(#manager{peek       = Peek,
                                handler_id = HID,
                                handlers   = Handlers} = M, Pid, Ref,
                       {start_handler, Sock}) ->
    i("try start handler (~w)", [HID]),
    case handler_start(HID, Sock, Peek) of
        {ok, {HPid, HMRef}} ->
            i("handler ~w started", [HID]),
            manager_reply(Pid, Ref, {ok, HPid}),
            M#manager{handler_id = HID+1,
                      handlers   = [{HPid, HMRef, HID}|Handlers]};
        {error, Reason} = ERROR ->
            e("Failed starting new handler: "
              "~n   Sock:   ~p"
              "~n   Reason: ~p", [Sock, Reason]),
            manager_reply(Pid, Ref, ERROR),
            M
    end;
manager_handle_request(#manager{acceptor = {Pid, MRef},
                                handlers = Handlers}, Pid, Ref,
                       {stop, Reason}) ->
    i("stop"),
    manager_reply(Pid, Ref, ok),
    manager_stop_handlers(Handlers, Reason),
    i("try stop acceptor ~p: ~p", [Pid, Reason]),
    erlang:demonitor(MRef, [flush]),
    acceptor_stop(Pid, Reason),
    i("stop", []),
    exit(Reason).


manager_stop_handlers(Handlers, Reason) ->
    lists:foreach(fun({P,M,ID}) -> 
                          manager_stop_handler(P, M, ID, Reason) 
                  end, Handlers).

manager_stop_handler(Pid, MRef, ID, Reason) ->
    i("try stop handler ~w (~p): ~p", [ID, Pid, Reason]),
    erlang:demonitor(MRef, [flush]),
    handler_stop(Pid, Reason),
    ok.

    

%% =========================================================================

acceptor_start(Domain, Type, Proto) ->
    Self = self(),
    A = {Pid, _} = spawn_monitor(fun() -> 
                                         acceptor_init(Self, Domain, Type, Proto) 
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


acceptor_init(Manager, Domain, Type, Proto) ->
    put(sname, "acceptor"),
    try acceptor_do_init(Domain, Type, Proto) of
        Sock ->
            Manager ! {acceptor, self(), ok},
            acceptor_loop(#acceptor{manager = Manager,
                                    socket  = Sock})
    catch
        throw:E:P ->
            e("Failed initiate: "
              "~n   Error: ~p"
              "~n   Path:  ~p", [E, P]),
            Manager ! {acceptor, self(), {error, {catched, E, P}}}
    end.

acceptor_do_init(Domain, Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    F = fun(X) -> case socket:getopt(Sock, socket, X) of
                      {ok, V} ->    f("~p", [V]);
                      {error, _} -> f("~w", [X])
                  end
        end,
    i("(socket) open (~s,~s,~s) - try find (local) address", 
      [F(domain), F(type), F(protocol)]),
    Addr = which_addr(Domain),
    SA = #{family => Domain,
           addr   => Addr},
    i("found (~p) - try (socket) bind", [Addr]),
    Port = case socket:bind(Sock, SA) of
               {ok, P} ->
                   P;
               {error, BReason} ->
                   throw({bind, BReason})
           end,
    i("bound (~w) - try (socket) listen (acceptconn: ~s)", 
      [Port, F(acceptconn)]),
    case socket:listen(Sock) of
        ok ->
            i("listening (acceptconn: ~s)", 
              [F(acceptconn)]),
            Sock;
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

which_addr2(inet = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 4) ->
    Addr;
which_addr2(inet6 = _Domain, [{addr, Addr}|_IFO]) when (size(Addr) =:= 8) ->
    Addr;
which_addr2(Domain, [_|IFO]) ->
    which_addr2(Domain, IFO).


acceptor_loop(#acceptor{socket = LSock} = A) ->
    i("try accept"),
    case socket:accept(LSock, infinity) of
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

handler_start(ID, Sock, Peek) ->
    Self = self(),
    H = {Pid, _} = spawn_monitor(fun() ->
                                         handler_init(Self, ID, Peek, Sock)
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


handler_init(Manager, ID, Peek, Sock) ->
    put(sname, f("handler:~w", [ID])),
    i("starting"),
    Manager ! {handler, self(), ok},
    receive
        {handler, Pid, Ref, continue} ->
            i("got continue"),
            handler_reply(Pid, Ref, ok),
            {ok, Domain} = socket:getopt(Sock, socket, domain),
            {ok, Type}   = socket:getopt(Sock, socket, type),
            {ok, Proto}  = socket:getopt(Sock, socket, protocol),
            i("got continue when: "
              "~n   Domain:   ~p"
              "~n   Type:     ~p"
              "~n   Protocol: ~p", [Domain, Type, Proto]),
            %% socket:setopt(Socket, otp, debug, true),
            handler_loop(#handler{peek    = Peek,
                                  manager = Manager,
                                  type    = Type,
                                  socket  = Sock})
    end.

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


send(#handler{socket = Sock, type = stream}, Msg, _) ->
    socket:send(Sock, Msg);
send(#handler{socket = Sock, type = dgram}, Msg, Dest) ->
    socket:sendto(Sock, Msg, Dest).



%% =========================================================================

f(F, A) ->
    ?LIB:f(F, A).

e(F, A) ->
    ?LIB:e(F, A).

i(F) ->
    ?LIB:i(F).

i(F, A) ->
    ?LIB:i(F, A).

