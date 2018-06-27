%%%-------------------------------------------------------------------
%%% @author Micael Karlberg <Micael.Karlberg@ericsson.com>
%%% @copyright (C) 2018, Micael Karlberg
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2018 by Micael Karlberg <Micael.Karlberg@ericsson.com>
%%%-------------------------------------------------------------------
-module(server).

-export([start/0]).

start() ->
    start_tcp().

start_tcp() ->
    start(inet, stream, tcp).

start(Domain, Type, Proto) ->
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
    accept_loop(LSock, []).

accept_loop(LSock, Socks) ->
    i("try accept"),
    case socket:accept(LSock, infinity) of
        {ok, Sock} ->
            i("accepted: ~p", [Sock]),
            accept_loop(LSock, [Sock|Socks]);
        {error, Reason} ->
            e("accept failure: "
              "~n   ~p", [Reason]),
            exit({accept, Reason})
    end.


e(F, A) ->
    p("<ERROR> " ++ F, A).

i(F) ->
    i(F, []).
i(F, A) ->
    p("*** " ++ F, A).

p(F, A) ->
    io:format("[server] " ++ F ++ "~n", A).
    
