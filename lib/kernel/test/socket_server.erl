%%%-------------------------------------------------------------------
%%% @author Micael Karlberg <Micael.Karlberg@ericsson.com>
%%% @copyright (C) 2018, Micael Karlberg
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2018 by Micael Karlberg <Micael.Karlberg@ericsson.com>
%%%-------------------------------------------------------------------
-module(socket_server).

-export([start/0]).

-define(REQ, 0).
-define(REP, 1).

-record(manager, {acceptor, handler_id, handlers}).
-record(acceptor, {socket, manager}).
-record(handler, {socket, manager}).

start() ->
    start_tcp().

start_tcp() ->
    start(inet, stream, tcp).

start(Domain, Type, Proto) ->
    put(sname, "starter"),
    i("try start manager"),
    {Pid, MRef} = manager_start(Domain, Type, Proto),
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

manager_start(Domain, Type, Proto) ->
    spawn_monitor(fun() -> manager_init(Domain, Type, Proto) end).

manager_start_handler(Pid, Sock) ->
    manager_request(Pid, {start_handler, Sock}).

manager_stop(Pid, Reason) ->
    manager_request(Pid, {stop, Reason}).

manager_request(Pid, Request) ->
    request(manager, Pid, Request).

manager_reply(Pid, Ref, Reply) ->
    reply(manager, Pid, Ref, Reply).


manager_init(Domain, Type, Proto) ->
    put(sname, "manager"),
    i("try start acceptor"),
    case acceptor_start(Domain, Type, Proto) of
        {ok, {Pid, MRef}} ->
            i("acceptor started"),
            manager_loop(#manager{acceptor   = {Pid, MRef},
                                  handler_id = 1,
                                  handlers   = []});
        {error, Reason} ->
            exit({failed_starting_acceptor, Reason})
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


manager_handle_request(#manager{handler_id = HID,
                                handlers   = Handlers} = M, Pid, Ref,
                       {start_handler, Sock}) ->
    i("try start handler (~w)", [HID]),
    case handler_start(HID, Sock) of
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
    i("(socket) open - try find (local) address"),
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
    i("bound (~w) - try (socket) listen", [Port]),
    case socket:listen(Sock) of
        ok ->
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
    i("try start handler"),
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

handler_start(ID, Sock) ->
    Self = self(),
    H = {Pid, _} = spawn_monitor(fun() -> handler_init(Self, ID, Sock) end),
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
    request(handler, Pid, Request).

handler_reply(Pid, Ref, Reply) ->
    reply(handler, Pid, Ref, Reply).


handler_init(Manager, ID, Sock) ->
    put(sname, f("handler:~w", [ID])),
    i("starting"),
    Manager ! {handler, self(), ok},
    receive
        {handler, Pid, Ref, continue} ->
            i("continue"),
            handler_reply(Pid, Ref, ok),
            %% socket:setopt(Socket, otp, debug, true),
            handler_loop(#handler{manager = Manager,
                                  socket  = Sock})
    end.

handler_loop(#handler{socket = Socket} = H) ->
    case socket:recv(Socket) of
        {ok, Msg} ->
            i("received ~w bytes of data", [size(Msg)]),
            case dec_msg(Msg) of
                {request, N, Req} ->
                    i("received request ~w: "
                      "~n   ~p", [N, Req]),
                    Reply = enc_rep_msg(N, "hoppsan"),
                    case socket:send(Socket, Reply) of
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



%% =========================================================================

enc_req_msg(N, Data) ->
    enc_msg(?REQ, N, Data).

enc_rep_msg(N, Data) ->
    enc_msg(?REP, N, Data).

enc_msg(Type, N, Data) when is_list(Data) ->
    enc_msg(Type, N, list_to_binary(Data));
enc_msg(Type, N, Data) 
  when is_integer(Type) andalso is_integer(N) andalso is_binary(Data) ->
    <<Type:32/integer, N:32/integer, Data/binary>>.
    
dec_msg(<<?REQ:32/integer, N:32/integer, Data/binary>>) ->
    {request, N, Data};
dec_msg(<<?REP:32/integer, N:32/integer, Data/binary>>) ->
    {reply, N, Data}.



%% ---

request(Tag, Pid, Request) ->
    Ref = make_ref(),
    Pid ! {Tag, self(), Ref, Request},
    receive
        {Tag, Pid, Ref, Reply} ->
            Reply
    end.
    
reply(Tag, Pid, Ref, Reply) ->
    Pid ! {Tag, self(), Ref, Reply}.


%% ---

formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp(Now) ->
    N2T = fun(N) -> calendar:now_to_local_time(N) end,
    format_timestamp(Now, N2T, true).

format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
    FormatExtra = ".~.2.0w",
    ArgsExtra   = [N3 div 10000],
    format_timestamp(N, N2T, FormatExtra, ArgsExtra);
format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
    FormatExtra = "",
    ArgsExtra   = [],
    format_timestamp(N, N2T, FormatExtra, ArgsExtra).

format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
    {Date, Time}   = N2T(N),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
                      [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
    lists:flatten(FormatDate).


%% ---

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

e(F, A) ->
    p("<ERROR> " ++ F, A).

i(F) ->
    i(F, []).
i(F, A) ->
    p("*** " ++ F, A).

p(F, A) ->
    p(get(sname), F, A).

p(SName, F, A) ->
    io:format("[~s,~p][~s] " ++ F ++ "~n", 
              [SName,self(),formated_timestamp()|A]).
    
