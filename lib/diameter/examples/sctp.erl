
-module(sctp).

%%
%% A small example demonstrating the establishment of an SCTP
%% association with gen_sctp.
%%

-export([assoc/0,
         dbg/0]).

-include_lib("kernel/include/inet_sctp.hrl").

-define(ADDR, {127,0,0,1}).
-define(SERVER_PORT, 3868).
-define(CONNECT_TIMEOUT, 2000).
-define(REQUEST, <<0:64>>).
-define(REPLY,   <<1:64>>).

-record(server, {client,
                 socket,
                 assoc}).

-record(client, {socket,
                 assoc}).

%% assoc/0
%%
%% Return on a successfully established association, raise an
%% exception otherwise.

assoc() ->
    {_, MRef} = spawn_monitor(fun server/0),
    receive {'DOWN', MRef, process, _, Info} -> Info end.

%% dbg/0

dbg() ->
    dbg:tracer(),
    dbg:p(all,c),
    dbg:tpl(?MODULE, [{'_',[],[{exception_trace}]}]),
    dbg:tp(gen_sctp, [{'_',[],[{exception_trace}]}]),
    ok.

%% server/0

server() ->
    {ok, Sock} = gen_sctp:open([binary,
                                {reuseaddr, true},
                                {active, true},
                                {ip, ?ADDR},
                                {port, ?SERVER_PORT}]),
    ok = gen_sctp:listen(Sock, true),
    {_Pid, MRef} = spawn_monitor(fun client/0),
    s(#server{client = MRef, socket = Sock}),
    gen_sctp:close(Sock).

%% s/1

s(#server{} = S) ->
    s(s(receive T -> T end, S));
s(T) ->
    T.

%% s/2

s({sctp, Sock, _RA, _RP, {[], #sctp_assoc_change{state = comm_up,
                                                 assoc_id = Id}}},
  #server{socket = Sock, assoc = undefined}
  = S) ->
    S#server{assoc = Id};

s({sctp, Sock, _RA, _RP, {[#sctp_sndrcvinfo{assoc_id = AId,
                                            stream = SId}],
                          ?REQUEST}},
  #server{socket = Sock, assoc = AId}
  = S) ->
    ok = gen_sctp:send(Sock, AId, SId, ?REPLY),
    S;

s({'DOWN', MRef, process, _, normal} = T, #server{client = MRef}) ->
    T.

%% client/0

client() ->
    {ok, Sock} = gen_sctp:open([binary,
                                {reuseaddr, true},
                                {active, true},
                                {ip, ?ADDR},
                                {port, 0}]),
    ok = gen_sctp:connect_init(Sock, ?ADDR, ?SERVER_PORT, []),
    c(#client{socket = Sock}),
    gen_sctp:close(Sock).


%% c/1

c(#client{} = S) ->
    c(c(receive T -> T end, S));
c(T) ->
    T.

c({sctp, Sock, _RA, _RP, {[], #sctp_assoc_change{state = comm_up,
                                                 assoc_id = Id}}},
  #client{socket = Sock, assoc = undefined}
  = S) ->
    ok = gen_sctp:send(Sock, Id, 0, ?REQUEST),
    S#client{assoc = Id};

c({sctp, Sock, _RA, _RP, {[#sctp_sndrcvinfo{assoc_id = AId}], ?REPLY}},
  #client{socket = Sock, assoc = AId}) ->
    ok.
