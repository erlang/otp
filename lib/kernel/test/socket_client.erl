%%%-------------------------------------------------------------------
%%% @author Micael Karlberg <Micael.Karlberg@ericsson.com>
%%% @copyright (C) 2018, Micael Karlberg
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2018 by Micael Karlberg <Micael.Karlberg@ericsson.com>
%%%-------------------------------------------------------------------
-module(client).

-export([start/1]).

start(Port) ->
    start_tcp(Port).

start_tcp(Port) ->
    start(inet, stream, tcp, Port).

start(Domain, Type, Proto, Port) ->
    try do_init(Domain, Type, Proto) of
        Sock ->
            connect(Sock, Domain, Port)
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
    i("try (socket) connect to ~p", [SA]),
    case socket:connect(Sock, SA) of
        ok ->
            i("connected"),
            ok;
        {error, Reason} ->
            e("connect failure: "
              "~n   ~p", [Reason]),
            exit({connect, Reason})
    end.


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



e(F, A) ->
    p("<ERROR> " ++ F, A).

i(F) ->
    i(F, []).
i(F, A) ->
    p("*** " ++ F, A).
    
p(F, A) ->
    io:format("[client] " ++ F ++ "~n", A).
    
