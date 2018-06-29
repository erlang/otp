%%%-------------------------------------------------------------------
%%% @author Micael Karlberg <Micael.Karlberg@ericsson.com>
%%% @copyright (C) 2018, Micael Karlberg
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2018 by Micael Karlberg <Micael.Karlberg@ericsson.com>
%%%-------------------------------------------------------------------
-module(socket_client).

-export([start/1]).

-define(REQ, 0).
-define(REP, 1).

start(Port) ->
    start_tcp(Port).

start_tcp(Port) ->
    start(inet, stream, tcp, Port).

start(Domain, Type, Proto, Port) ->
    try do_init(Domain, Type, Proto) of
        Sock ->
            connect(Sock, Domain, Port),
            %% Give the server some time...
            p("wait some", []),
            %% sleep(5000),
            %% ok = socket:close(Sock),
            send_loop(Sock)
    catch
        throw:E ->
            e("Failed initiate: "
              "~n   Error: ~p", [E])
    end.

do_init(Domain, Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    i("try (socket) bind"),
    case socket:bind(Sock, any) of
        {ok, _P} ->
            Sock;
        {error, BReason} ->
            throw({bind, BReason})
    end.

which_addr(Domain) ->
    Iflist = case inet:getifaddrs() of
                 {ok, IFL} ->
                     IFL;
                 {error, Reason} ->
                     throw({inet,getifaddrs,Reason})
             end,
    which_addr(Domain, Iflist).


connect(Sock, Domain, Port) ->
    Addr = which_addr(Domain),
    SA = #{family => Domain,
           addr   => Addr,
           port   => Port},
    i("try (socket) connect to:"
      "~n   ~p", [SA]),
    case socket:connect(Sock, SA) of
        ok ->
            i("connected"),
            ok;
        {error, Reason} ->
            e("connect failure: "
              "~n   ~p", [Reason]),
            exit({connect, Reason})
    end.


send_loop(Sock) ->
    send_loop(Sock, 1).

send_loop(Sock, N) when (N =< 10) ->
    i("try send request ~w", [N]),
    Req = enc_req_msg(N, "hejsan"),
    case socket:send(Sock, Req) of
        ok ->
            i("request ~w sent - now try read answer", [N]),
            case socket:recv(Sock, 0) of
                {ok, Msg} ->
                    i("received ~w bytes of data", [size(Msg)]),
                    case dec_msg(Msg) of
                        {reply, N, Reply} ->
                            i("received reply ~w: ~p", [N, Reply]),
                            send_loop(Sock, N+1)
                    end;
                {error, RReason} ->
                    e("Failed recv response for request ~w: "
                      "~n   ~p", [N, RReason]),
                    exit({failed_recv, RReason})
            end;
        {error, SReason} ->
            e("Failed send request ~w: "
              "~n   ~p", [SReason]),
            exit({failed_send, SReason})
    end;
send_loop(Sock, _N) ->
    i("we are done - close the socket when: "
      "~n   ~p", [socket:info()]),
    ok = socket:close(Sock),
    i("we are done - socket closed when: "
      "~n   ~p", [socket:info()]).


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

sleep(T) ->
    receive after T -> ok end.

                     
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
    io:format("[client,~p][~s] " ++ F ++ "~n", [self(),formated_timestamp()|A]).
    
