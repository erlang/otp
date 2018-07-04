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

-module(socket_client).

-export([
         start/1,
         start_tcp/1, start_tcp/2,
         start_udp/1, start_udp/2
        ]).

-define(LIB, socket_lib).

-record(client, {socket, type, dest, msg_id = 1}).

start(Port) ->
    start_tcp(Port).

start_tcp(Port) ->
    start(inet, stream, tcp, Port).

start_tcp(Addr, Port) when (size(Addr) =:= 4) ->
    start(inet, stream, tcp, Addr, Port);
start_tcp(Addr, Port) when (size(Addr) =:= 8) ->
    start(inet6, stream, tcp, Addr, Port).


start_udp(Port) ->
    start(inet, dgram, udp, Port).

start_udp(Addr, Port) when (size(Addr) =:= 4) ->
    start(inet, dgram, udp, Addr, Port);
start_udp(Addr, Port) when (size(Addr) =:= 8) ->
    start(inet6, dgram, udp, Addr, Port).


start(Domain, Type, Proto, Port) ->
    start(Domain, Type, Proto, which_addr(Domain), Port).

start(Domain, Type, Proto, Addr, Port) ->
    put(sname, "starter"),
    SA = #{family => Domain,
           addr   => Addr,
           port   => Port},
    do_start(Domain, Type, Proto, SA).

do_start(Domain, stream = Type, Proto, SA) ->
    try do_init(Domain, Type, Proto) of
        Sock ->
            connect(Sock, SA),
            i("connected: "
              "~n   From: ~p"
              "~n   To:   ~p", 
              [
               case socket:sockname(Sock) of
                   {ok, Name} -> Name;
                   {error, _} = NE -> NE
               end,
               case socket:peername(Sock) of
                   {ok, Name} -> Name;
                   {error, _} = PE -> PE
               end
              ]),
            %% Give the server some time...
            i("wait some", []),
            ?LIB:sleep(5000),
            %% ok = socket:close(Sock),
            send_loop(#client{socket = Sock, 
                              type   = Type})
    catch
        throw:E ->
            e("Failed initiate: "
              "~n   Error: ~p", [E])
    end;
do_start(Domain, dgram = Type, Proto, SA) ->
    try do_init(Domain, Type, Proto) of
        Sock ->
            %% Give the server some time...
            i("wait some", []),
            ?LIB:sleep(5000),
            %% ok = socket:close(Sock),
            send_loop(#client{socket = Sock, 
                              type   = Type,
                              dest   = SA})
    catch
        throw:E ->
            e("Failed initiate: "
              "~n   Error: ~p", [E])
    end.


do_init(Domain, stream = Type, Proto) ->
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
    end;
do_init(Domain, dgram = Type, Proto) ->
    i("try (socket) open"),
    Sock = case socket:open(Domain, Type, Proto) of
               {ok, S} ->
                   S;
               {error, OReason} ->
                   throw({open, OReason})
           end,
    case socket:bind(Sock, any) of
        {ok, _} ->
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


connect(Sock, SA) ->
    i("try (socket) connect to:"
      "~n   ~p", [SA]),
    case socket:connect(Sock, SA) of
        ok ->
            ok;
        {error, Reason} ->
            e("connect failure: "
              "~n   ~p", [Reason]),
            exit({connect, Reason})
    end.


send_loop(#client{msg_id = N} = C) when (N =< 10) ->
    i("try send request ~w", [N]),
    Req = ?LIB:enc_req_msg(N, "hejsan"),
    case send(C, Req) of
        ok ->
            i("request ~w sent - now try read answer", [N]),
            case recv(C) of
                {ok, {Source, Msg}} ->
                    i("received ~w bytes of data~s", 
                      [size(Msg),  case Source of
                                       undefined -> "";
                                       _ -> ?LIB:f(" from:~n   ~p", [Source])
                                   end]),
                    case ?LIB:dec_msg(Msg) of
                        {reply, N, Reply} ->
                            i("received reply ~w: ~p", [N, Reply]),
                            send_loop(C#client{msg_id = N+1})
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
send_loop(#client{socket = Sock}) ->
    i("we are done - close the socket when: "
      "~n   ~p", [socket:info()]),
    ok = socket:close(Sock),
    i("we are done - socket closed when: "
      "~n   ~p", [socket:info()]).


send(#client{socket = Sock, type = stream}, Msg) ->
    socket:send(Sock, Msg);
send(#client{socket = Sock, type = dgram, dest = Dest}, Msg) ->
    %% i("try send to: "
    %%   "~n   ~p", [Dest]),
    %% ok = socket:setopt(Sock, otp, debug, true),
    socket:sendto(Sock, Msg, Dest).

recv(#client{socket = Sock, type = stream}) ->
    case socket:recv(Sock) of
        {ok, Msg} ->
            {ok, {undefined, Msg}};
        {error, _} = ERROR ->
            ERROR
    end;
recv(#client{socket = Sock, type = dgram}) ->
    socket:recvfrom(Sock).


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

%% enc_req_msg(N, Data) ->
%%     enc_msg(?REQ, N, Data).

%% enc_rep_msg(N, Data) ->
%%     enc_msg(?REP, N, Data).

%% enc_msg(Type, N, Data) when is_list(Data) ->
%%     enc_msg(Type, N, list_to_binary(Data));
%% enc_msg(Type, N, Data) 
%%   when is_integer(Type) andalso is_integer(N) andalso is_binary(Data) ->
%%     <<Type:32/integer, N:32/integer, Data/binary>>.
    
%% dec_msg(<<?REQ:32/integer, N:32/integer, Data/binary>>) ->
%%     {request, N, Data};
%% dec_msg(<<?REP:32/integer, N:32/integer, Data/binary>>) ->
%%     {reply, N, Data}.


%% ---

%% sleep(T) ->
%%     receive after T -> ok end.

                     
%% ---

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(Now) ->
%%     N2T = fun(N) -> calendar:now_to_local_time(N) end,
%%     format_timestamp(Now, N2T, true).

%% format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
%%     FormatExtra = ".~.2.0w",
%%     ArgsExtra   = [N3 div 10000],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra);
%% format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
%%     FormatExtra = "",
%%     ArgsExtra   = [],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra).

%% format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
%%     {Date, Time}   = N2T(N),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate =
%%         io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
%%                       [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
%%     lists:flatten(FormatDate).


%% ---

e(F, A) ->
    ?LIB:e(F, A).

i(F) ->
    ?LIB:i(F).

i(F, A) ->
    ?LIB:i(F, A).
    
