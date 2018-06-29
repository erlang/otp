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

-record(handler, {socket, parent}).

start() ->
    start_tcp().

start_tcp() ->
    start(inet, stream, tcp).

start(Domain, Type, Proto) ->
    put(sname, "starter"),
    try do_init(Domain, Type, Proto) of
        Sock ->
            accept_loop(Sock)
    catch
        throw:E:P ->
            e("Failed initiate: "
              "~n   Error: ~p"
              "~n   Path:  ~p", [E, P])
    end.

do_init(Domain, Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    i("opened - now try find (local) address"),
    Addr = which_addr(Domain),
    SA = #{family => Domain,
           addr   => Addr},
    i("addr ~p - now try (socket) bind", [Addr]),
    Port = case socket:bind(Sock, SA) of
               {ok, P} ->
                   P;
               {error, BReason} ->
                   throw({bind, BReason})
           end,
    i("bound to ~w - now try (socket) listen", [Port]),
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


accept_loop(LSock) ->
    put(sname, "acceptor"),
    accept_loop(LSock, []).

accept_loop(LSock, Handlers) ->
    i("try accept"),
    case socket:accept(LSock, infinity) of
        {ok, Sock} ->
            i("accepted: "
              "~n   ~p"
              "~nwhen"
              "~n   ~p", [Sock, socket:info()]),
            case handle_accept_success(Sock) of
                {ok, Handler} ->
                    accept_loop(LSock, [Handler|Handlers]);
                {error, HReason} ->
                    e("Failed starting handler: "
                      "~n   ~p", [HReason]),
                    socket:close(Sock),
                    exit({failed_starting_handler, HReason})
            end;
        {error, Reason} ->
            e("accept failure: "
              "~n   ~p", [Reason]),
            exit({accept, Reason})
    end.


handle_accept_success(Sock) ->
    Self    = self(),
    Handler = spawn_link(fun() -> handler_init(Self, Sock) end),
    case socket:setopt(Sock, otp, controlling_process, Handler) of
        ok ->
            %% Normally we should have a msgs collection here
            %% (of messages we receive before the control was
            %% handled over to Handler), but since we don't 
            %% have active implemented yet...
            handler_continue(Handler),
            {ok, {Handler, Sock}};
       {error, _} = ERROR ->
            exit(Handler, kill),
            ERROR
    end.


handler_init(Parent, Socket) ->
    put(sname, "handler"),
    receive
        {handler, Parent, continue} ->
            socket:setopt(Socket, otp, debug, true),
            handler_loop(#handler{parent = Parent,
                                  socket = Socket})
    end.

handler_continue(Handler) ->
    Handler ! {handler, self(), continue}.

handler_loop(#handler{socket = Socket} = H) ->
    case socket:recv(Socket, 0) of
        {ok, Msg} when (size(Msg) =:= 0) ->
            i("received empty msg - hickup? - try again", []),
            handler_loop(H);
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
            
    
%% ---

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

e(F, A) ->
    p("<ERROR> " ++ F, A).

i(F) ->
    i(F, []).
i(F, A) ->
    p("*** " ++ F, A).

p(F, A) ->
    p(get(sname), F, A).

p(SName, F, A) ->
    io:format("[server:~s,~p][~s] " ++ F ++ "~n", 
              [SName,self(),formated_timestamp()|A]).
    
