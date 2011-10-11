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

%%
%% Tests of diameter_tcp/sctp as implementations of the diameter
%% transport interface.
%%

-module(diameter_transport_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([start/1,
         tcp_accept/1,
         tcp_connect/1,
         sctp_accept/1,
         sctp_connect/1,
         stop/1]).

-export([accept/1,
         connect/1,
         init/2]).

-include_lib("kernel/include/inet_sctp.hrl").
-include_lib("diameter/include/diameter.hrl").
-include("diameter_ct.hrl").

-define(util, diameter_util).

%% Corresponding to diameter_* transport modules.
-define(TRANSPORTS, [tcp, sctp]).

%% Receive a message.
-define(RECV(Pat, Ret), receive Pat -> Ret end).
-define(RECV(Pat), ?RECV(Pat, now())).

%% Or not.
-define(WAIT(Ms), receive after Ms -> now() end).

%% Sockets are opened on the loopback address.
-define(ADDR, {127,0,0,1}).

%% diameter_tcp doesn't use anything but host_ip_address, and that
%% only is a local address isn't configured as at transport start.
-define(SVC(Addrs), #diameter_service{capabilities
                                      = #diameter_caps{host_ip_address
                                                       = Addrs}}).

%% The term diameter_tcp/sctp registers after opening a listening
%% socket. This is an implementation detail that should probably be
%% replaced by some documented way of getting at the port number of
%% the listening socket, which is what we're after since we specify
%% port 0 to get something unused.
-define(TCP_LISTENER(Ref, Addr, LSock),
        {diameter_tcp, listener, {Ref, {Addr, LSock}}}).
-define(SCTP_LISTENER(Ref, Addr, LSock),
        {diameter_sctp, listener, {Ref, {[Addr], LSock}}}).

%% The term we register after open a listening port with gen_tcp.
-define(TEST_LISTENER(Ref, PortNr),
        {?MODULE, listen, Ref, PortNr}).

%% Message over the transport interface.
-define(TMSG(T), {diameter, T}).

%% Options for gen_tcp/gen_sctp.
-define(TCP_OPTS,  [binary, {active, true}, {packet, 0}]).
-define(SCTP_OPTS, [binary, {active, true}, {sctp_initmsg, ?SCTP_INIT}]).

%% Request a specific number of streams just because we can.
-define(SCTP_INIT, #sctp_initmsg{num_ostreams = 5,
                                 max_instreams = 5}).

%% Messages from gen_sctp.
-define(SCTP(Sock, Data), {sctp, Sock, _, _, Data}).

%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [start | tc()] ++ [{group, all}, stop].

groups() ->
    [{all, [parallel], tc()}].

tc() ->
    [tcp_accept,
     tcp_connect,
     sctp_accept,
     sctp_connect].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

init_per_suite(Config) ->
    [{sctp, have_sctp()} | Config].

end_per_suite(_Config) ->
    ok.

%% ===========================================================================

start(_Config) ->
    ok = diameter:start().

stop(_Config) ->
    ok = diameter:stop().

%% ===========================================================================
%% tcp_accept/1
%% sctp_accept/1
%%
%% diameter transport accepting, test code connecting.

tcp_accept(_) ->
    accept(tcp).

sctp_accept(Config) ->
    if_sctp(fun accept/1, Config).

%% Start multiple accepting transport processes that are connected to
%% with an equal number of connecting processes using gen_tcp/sctp
%% directly.

-define(PEER_COUNT, 8).

accept(Prot) ->
    T = {Prot, make_ref()},
    [] = ?util:run(?util:scramble(acc(2*?PEER_COUNT, T, []))).

acc(0, _, Acc) ->
    Acc;
acc(N, T, Acc) ->
    acc(N-1, T, [{?MODULE, [init,
                            element(1 + N rem 2, {accept, gen_connect}),
                            T]}
                 | Acc]).

%% ===========================================================================
%% tcp_connect/1
%% sctp_connect/1
%%
%% Test code accepting, diameter transport connecting.

tcp_connect(_) ->
    connect(tcp).

sctp_connect(Config) ->
    if_sctp(fun connect/1, Config).

connect(Prot) ->
    T = {Prot, make_ref()},
    [] = ?util:run([{?MODULE, [init, X, T]} || X <- [gen_accept, connect]]).

%% ===========================================================================
%% ===========================================================================

%% have_sctp/0

have_sctp() ->
    try gen_sctp:open() of
        {ok, Sock} ->
            gen_sctp:close(Sock),
            true;
        {error, eprotonosupport} ->  %% fail on any other reason
            false
    catch
        error: badarg ->
            false
    end.

%% if_sctp/2

if_sctp(F, Config) ->
    case proplists:get_value(sctp, Config) of
        true ->
            F(sctp);
        false ->
            {skip, no_sctp}
    end.

%% init/2

init(accept, {Prot, Ref}) ->
    %% Start an accepting transport and receive notification of a
    %% connection.
    TPid = start_accept(Prot, Ref),

    %% Receive a message and send it back.
    <<_:8, Len:24, _/binary>> = Bin = bin(Prot, ?RECV(?TMSG({recv, P}), P)),

    Len = size(Bin),
    TPid ! ?TMSG({send, Bin}),

    %% Expect the transport process to die as a result of the peer
    %% closing the connection.
    MRef = erlang:monitor(process, TPid),
    ?RECV({'DOWN', MRef, process, _, _});

init(gen_connect, {Prot, Ref}) ->
    %% Lookup the peer's listening socket.
    {ok, PortNr} = inet:port(lsock(Prot, Ref)),

    %% Connect, send a message and receive it back.
    {ok, Sock} = gen_connect(Prot, PortNr, Ref),
    Bin = make_msg(),
    ok = gen_send(Prot, Sock, Bin),
    Bin = gen_recv(Prot, Sock);

init(gen_accept, {Prot, Ref}) ->
    %% Open a listening socket and publish the port number.
    {ok, LSock} = gen_listen(Prot),
    {ok, PortNr} = inet:port(LSock),
    true = diameter_reg:add_new(?TEST_LISTENER(Ref, PortNr)),

    %% Accept a connection, receive a message and send it back.
    {ok, Sock} = gen_accept(Prot, LSock),
    Bin = gen_recv(Prot, Sock),
    ok = gen_send(Prot, Sock, Bin);

init(connect, {Prot, Ref}) ->
    %% Lookup the peer's listening socket.
    [{?TEST_LISTENER(_, PortNr), _}] = match(?TEST_LISTENER(Ref, '_')),

    %% Start a connecting transport and receive notification of
    %% the connection.
    TPid = start_connect(Prot, PortNr, Ref),

    %% Send a message and receive it back.
    Bin = make_msg(),
    TPid ! ?TMSG({send, Bin}),
    Bin = bin(Prot, ?RECV(?TMSG({recv, P}), P)),

    %% Expect the transport process to die as a result of the peer
    %% closing the connection.
    MRef = erlang:monitor(process, TPid),
    ?RECV({'DOWN', MRef, process, _, _}).

lsock(sctp, Ref) ->
    [{?SCTP_LISTENER(_ , _, LSock), _}]
        = match(?SCTP_LISTENER(Ref, ?ADDR, '_')),
    LSock;
lsock(tcp, Ref) ->
    [{?TCP_LISTENER(_ , _, LSock), _}]
        = match(?TCP_LISTENER(Ref, ?ADDR, '_')),
    LSock.

match(Pat) ->
    case diameter_reg:match(Pat) of
        [] ->
            ?WAIT(50),
            match(Pat);
        L ->
            L
    end.

bin(sctp, #diameter_packet{bin = Bin}) ->
    Bin;
bin(tcp, Bin) ->
    Bin.

%% make_msg/0
%%
%% A valid Diameter message in as far as diameter_tcp examines it,
%% the module examining the length in the Diameter header to locate
%% message boundaries.

make_msg() ->
    N = 1024,
    Bin = rand_bytes(4*N),
    Len = 4*(N+1),
    <<1:8, Len:24, Bin/binary>>.

%% crypto:rand_bytes/1 isn't available on all platforms (since openssl
%% isn't) so roll our own.
rand_bytes(N) ->
    random:seed(now()),
    rand_bytes(N, <<>>).

rand_bytes(0, Bin) ->
    Bin;
rand_bytes(N, Bin) ->
    Oct = random:uniform(256) - 1,
    rand_bytes(N-1, <<Oct, Bin/binary>>).

%% ===========================================================================

%% start_connect/3

start_connect(Prot, PortNr, Ref) ->
    {ok, TPid, [?ADDR]} = start_connect(Prot,
                                        {connect, Ref},
                                        ?SVC([]),
                                        [{raddr, ?ADDR},
                                         {rport, PortNr},
                                         {ip, ?ADDR},
                                         {port, 0}]),
    ?RECV(?TMSG({TPid, connected, _})),
    TPid.

start_connect(sctp, T, Svc, Opts) ->
    diameter_sctp:start(T, Svc, [{sctp_initmsg, ?SCTP_INIT} | Opts]);
start_connect(tcp, T, Svc, Opts) ->
    diameter_tcp:start(T, Svc, Opts).

%% start_accept/2
%%
%% Start transports sequentially by having each wait for a message
%% from a job in a queue before commencing. Only one transport with
%% a pending accept is started at a time since diameter_sctp currently
%% assumes (and diameter currently implements) this.

start_accept(Prot, Ref) ->
    Pid = sync(accept, Ref),

    %% Configure the same port number for transports on the same
    %% reference.
    PortNr = portnr(Prot, Ref),
    {Mod, Opts} = tmod(Prot),

    try
        {ok, TPid, [?ADDR]} = Mod:start({accept, Ref},
                                        ?SVC([?ADDR]),
                                        [{port, PortNr} | Opts]),
        ?RECV(?TMSG({TPid, connected})),
        TPid
    after
        Pid ! Ref
    end.

sync(What, Ref) ->
    ok = diameter_sync:cast({?MODULE, What, Ref},
                            [fun lock/2, Ref, self()],
                            infinity,
                            infinity),
    receive {start, Ref, Pid} -> Pid end.

lock(Ref, Pid) ->
    Pid ! {start, Ref, self()},
    erlang:monitor(process, Pid),
    Ref = receive T -> T end.

tmod(sctp) ->
    {diameter_sctp, [{sctp_initmsg, ?SCTP_INIT}]};
tmod(tcp) ->
    {diameter_tcp, []}.

portnr(sctp, Ref) ->
    case diameter_reg:match(?SCTP_LISTENER(Ref, ?ADDR, '_')) of
        [{?SCTP_LISTENER(_, _, LSock), _}] ->
            {ok, N} = inet:port(LSock),
            N;
        [] ->
            0
    end;
portnr(tcp, Ref) ->
    case diameter_reg:match(?TCP_LISTENER(Ref, ?ADDR, '_')) of
        [{?TCP_LISTENER(_, _, LSock), _}] ->
            {ok, N} = inet:port(LSock),
            N;
        [] ->
            0
    end.

%% ===========================================================================

%% gen_connect/3

gen_connect(Prot, PortNr, Ref) ->
    Pid = sync(connect, Ref),

    %% Stagger connect attempts to avoid the situation that no
    %% transport process is accepting yet.
    receive after 250 -> ok end,

    try
        gen_connect(Prot, PortNr)
    after
        Pid ! Ref
    end.

gen_connect(sctp = P, PortNr) ->
    {ok, Sock} = Ok = gen_sctp:open([{ip, ?ADDR}, {port, 0} | ?SCTP_OPTS]),
    ok = gen_sctp:connect_init(Sock, ?ADDR, PortNr, []),
    Ok = gen_accept(P, Sock);
gen_connect(tcp, PortNr) ->
    gen_tcp:connect(?ADDR, PortNr, ?TCP_OPTS).

%% gen_listen/1

gen_listen(sctp) ->
    {ok, Sock} = gen_sctp:open([{ip, ?ADDR}, {port, 0} | ?SCTP_OPTS]),
    {gen_sctp:listen(Sock, true), Sock};
gen_listen(tcp) ->
    gen_tcp:listen(0, [{ip, ?ADDR} | ?TCP_OPTS]).

%% gen_accept/2

gen_accept(sctp, Sock) ->
    Assoc = ?RECV(?SCTP(Sock, {_, #sctp_assoc_change{state = comm_up,
                                                     outbound_streams = O,
                                                     inbound_streams = I,
                                                     assoc_id = A}}),
                  {O, I, A}),
    putr(assoc, Assoc),
    {ok, Sock};
gen_accept(tcp, LSock) ->
    gen_tcp:accept(LSock).

%% gen_send/3

gen_send(sctp, Sock, Bin) ->
    {OS, _IS, Id} = getr(assoc),
    {_, _, Us} = now(),
    gen_sctp:send(Sock, Id, Us rem OS, Bin);
gen_send(tcp, Sock, Bin) ->
    gen_tcp:send(Sock, Bin).

%% gen_recv/2

gen_recv(sctp, Sock) ->
    {_OS, _IS, Id} = getr(assoc),
    ?RECV(?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = Id}], Bin}), Bin);
gen_recv(tcp, Sock) ->
    tcp_recv(Sock, <<>>).

tcp_recv(_, <<_:8, Len:24, _/binary>> = Bin)
  when Len =< size(Bin) ->
    Bin;
tcp_recv(Sock, B) ->
    receive {tcp, Sock, Bin} -> tcp_recv(Sock, <<B/binary, Bin/binary>>) end.

%% putr/2

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

%% getr/1

getr(Key) ->
    get({?MODULE, Key}).
