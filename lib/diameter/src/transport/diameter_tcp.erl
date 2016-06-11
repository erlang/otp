%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(diameter_tcp).
-dialyzer({no_fail_call, throttle/2}).

-behaviour(gen_server).

%% interface
-export([start/3]).

%% child start from supervisor
-export([start_link/1]).

%% child start from here
-export([init/1]).

%% gen_server callbacks
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-export([listener/1,%% diameter_sync callback
         info/1]).  %% service_info callback

-export([ports/0,
         ports/1]).

-export_type([connect_option/0,
              listen_option/0]).

-include_lib("diameter/include/diameter.hrl").

%% Keys into process dictionary.
-define(INFO_KEY, info).
-define(REF_KEY,  ref).

-define(ERROR(T), erlang:error({T, ?MODULE, ?LINE})).

-define(DEFAULT_PORT, 3868).  %% RFC 3588, ch 2.1
-define(DEFAULT_FRAGMENT_TIMEOUT, 1000).

-define(IS_UINT32(N), (is_integer(N) andalso 0 =< N andalso 0 == N bsr 32)).
-define(IS_TIMEOUT(N), (infinity == N orelse ?IS_UINT32(N))).

%% cb_info passed to ssl.
-define(TCP_CB(Mod), {Mod, tcp, tcp_closed, tcp_error}).

%% The same gen_server implementation supports three different kinds
%% of processes: an actual transport process, one that will club it to
%% death should the parent die before a connection is established, and
%% a process owning the listening port.

%% Listener process state.
-record(listener, {socket    :: inet:socket(),
                   count = 1 :: non_neg_integer()}).  %% accepting processes
%% The count of accepting processes was previously used to terminate
%% the listening process, but diameter_reg:subscribe/2 is now used for
%% this. Leave the the count for trace purposes.

%% Monitor process state.
-record(monitor,
        {parent             :: pid(),
         transport = self() :: pid()}).

-type length() :: 0..16#FFFFFF. %% message length from Diameter header
-type size()   :: non_neg_integer().  %% accumulated binary size
-type frag()   :: {length(), size(), binary(), list(binary())}
                | binary().

-type connect_option() :: {raddr, inet:ip_address()}
                        | {rport, pos_integer()}
                        | {ssl_options, true | [ssl:connect_option()]}
                        | option()
                        | ssl:connect_option()
                        | gen_tcp:connect_option().

-type match() :: inet:ip_address()
               | string()
               | [match()].

-type listen_option() :: {accept, match()}
                       | {ssl_options, true | [ssl:listen_option()]}
                       | ssl:listen_option()
                       | gen_tcp:listen_option().

-type option() :: {port, non_neg_integer()}
                | {fragment_timer, 0..16#FFFFFFFF}
                | {throttle_cb, diameter:evaluable()}.

%% Accepting/connecting transport process state.
-record(transport,
        {socket  :: inet:socket() | ssl:sslsocket(), %% accept/connect socket
         parent  :: pid(),          %% of process that started us
         module  :: module(),       %% gen_tcp-like module
         frag = <<>> :: frag(),     %% message fragment
         ssl     :: [term()] | boolean(),       %% ssl options, ssl or not
         timeout :: infinity | 0..16#FFFFFFFF,  %% fragment timeout
         tref = false  :: false | reference(),  %% fragment timer reference
         flush = false :: boolean(),            %% flush fragment at timeout?
         throttle_cb :: false | diameter:evaluable(), %% ask to receive
         throttled :: boolean() | binary()}).   %% stopped receiving?

%% The usual transport using gen_tcp can be replaced by anything
%% sufficiently gen_tcp-like by passing a 'module' option as the first
%% (for simplicity) transport option. The transport_module diameter_etcp
%% uses this to set itself as the module to call, its start/3 just
%% calling start/3 here with the option set.

%% ---------------------------------------------------------------------------
%% # start/3
%% ---------------------------------------------------------------------------

-spec start({accept, Ref}, #diameter_service{}, [listen_option()])
   -> {ok, pid(), [inet:ip_address()]}
 when Ref :: diameter:transport_ref();
           ({connect, Ref}, #diameter_service{}, [connect_option()])
   -> {ok, pid(), [inet:ip_address()]}
    | {ok, pid()}
 when Ref :: diameter:transport_ref().

start({T, Ref}, #diameter_service{capabilities = Caps}, Opts) ->
    diameter_tcp_sup:start(),  %% start tcp supervisors on demand
    {Mod, Rest} = split(Opts),
    Addrs = Caps#diameter_caps.host_ip_address,
    Arg = {T, Ref, Mod, self(), Rest, Addrs},
    diameter_tcp_sup:start_child(Arg).

split([{module, M} | Opts]) ->
    {M, Opts};
split(Opts) ->
    {gen_tcp, Opts}.

%% start_link/1

start_link(T) ->
    proc_lib:start_link(?MODULE,
                        init,
                        [T],
                        infinity,
                        diameter_lib:spawn_opts(server, [])).

%% ---------------------------------------------------------------------------
%% # info/1
%% ---------------------------------------------------------------------------

info({Mod, Sock}) ->
    lists:flatmap(fun(K) -> info(Mod, K, Sock) end,
                  [{socket, fun sockname/2},
                   {peer, fun peername/2},
                   {statistics, fun getstat/2}
                   | ssl_info(Mod, Sock)]).

info(Mod, {K,F}, Sock) ->
    case F(Mod, Sock) of
        {ok, V} ->
            [{K,V}];
        _ ->
            []
    end.

ssl_info(ssl = M, Sock) ->
    [{M, ssl_info(Sock)}];
ssl_info(_, _) ->
    [].

ssl_info(Sock) ->
    [{peercert, C} || {ok, C} <- [ssl:peercert(Sock)]].

%% ---------------------------------------------------------------------------
%% # init/1
%% ---------------------------------------------------------------------------

init(T) ->
    gen_server:enter_loop(?MODULE, [], i(T)).

%% i/1

%% A transport process.
i({T, Ref, Mod, Pid, Opts, Addrs})
  when T == accept;
       T == connect ->
    monitor(process, Pid),
    %% Since accept/connect might block indefinitely, spawn a process
    %% that does nothing but kill us with the parent until call
    %% returns.
    {ok, MPid} = diameter_tcp_sup:start_child(#monitor{parent = Pid}),
    {[SO|TO], Rest} = proplists:split(Opts, [ssl_options,
                                             fragment_timer,
                                             throttle_cb]),
    SslOpts = ssl_opts(SO),
    OwnOpts = lists:append(TO),
    Tmo = proplists:get_value(fragment_timer,
                              OwnOpts,
                              ?DEFAULT_FRAGMENT_TIMEOUT),
    ?IS_TIMEOUT(Tmo) orelse ?ERROR({fragment_timer, Tmo}),
    Throttle = proplists:get_value(throttle_cb, OwnOpts, false),
    Sock = init(T, Ref, Mod, Pid, SslOpts, Rest, Addrs),
    MPid ! {stop, self()},  %% tell the monitor to die
    M = if SslOpts -> ssl; true -> Mod end,
    putr(?REF_KEY, Ref),
    throttle(#transport{parent = Pid,
                        module = M,
                        socket = Sock,
                        ssl = SslOpts,
                        timeout = Tmo,
                        throttle_cb = Throttle,
                        throttled = false /= Throttle});
%% Put the reference in the process dictionary since we now use it
%% advertise the ssl socket after TLS upgrade.

%% A monitor process to kill the transport if the parent dies.
i(#monitor{parent = Pid, transport = TPid} = S) ->
    proc_lib:init_ack({ok, self()}),
    monitor(process, Pid),
    monitor(process, TPid),
    S;
%% In principle a link between the transport and killer processes
%% could do the same thing: have the accepting/connecting process be
%% killed when the killer process dies as a consequence of parent
%% death. However, a link can be unlinked and this is exactly what
%% gen_tcp seems to so. Links should be left to supervisors.

i({listen, LRef, APid, {Mod, Opts, Addrs}}) ->
    [_] = diameter_config:subscribe(LRef, transport), %% assert existence
    {[LA, LP], Rest} = proplists:split(Opts, [ip, port]),
    LAddrOpt = get_addr(LA, Addrs),
    LPort = get_port(LP),
    {ok, LSock} = Mod:listen(LPort, gen_opts(LAddrOpt, Rest)),
    LAddr = laddr(LAddrOpt, Mod, LSock),
    true = diameter_reg:add_new({?MODULE, listener, {LRef, {LAddr, LSock}}}),
    proc_lib:init_ack({ok, self(), {LAddr, LSock}}),
    monitor(process, APid),
    #listener{socket = LSock}.

laddr([], Mod, Sock) ->
    {ok, {Addr, _Port}} = sockname(Mod, Sock),
    Addr;
laddr([{ip, Addr}], _, _) ->
    Addr.

ssl_opts([]) ->
    false;
ssl_opts([{ssl_options, true}]) ->
    true;
ssl_opts([{ssl_options, Opts}])
  when is_list(Opts) ->
    Opts;
ssl_opts(T) ->
    ?ERROR({ssl_options, T}).

%% init/7

%% Establish a TLS connection before capabilities exchange ...
init(Type, Ref, Mod, Pid, true, Opts, Addrs) ->
    init(Type, Ref, ssl, Pid, [{cb_info, ?TCP_CB(Mod)} | Opts], Addrs);

%% ... or not.
init(Type, Ref, Mod, Pid, _, Opts, Addrs) ->
    init(Type, Ref, Mod, Pid, Opts, Addrs).

%% init/6

init(accept = T, Ref, Mod, Pid, Opts, Addrs) ->
    {[Matches], Rest} = proplists:split(Opts, [accept]),
    {LAddr, LSock} = listener(Ref, {Mod, Rest, Addrs}),
    proc_lib:init_ack({ok, self(), [LAddr]}),
    Sock = ok(accept(Mod, LSock)),
    ok = accept_peer(Mod, Sock, accept(Matches)),
    publish(Mod, T, Ref, Sock),
    diameter_peer:up(Pid),
    Sock;

init(connect = T, Ref, Mod, Pid, Opts, Addrs) ->
    {[LA, RA, RP], Rest} = proplists:split(Opts, [ip, raddr, rport]),
    LAddrOpt = get_addr(LA, Addrs),
    RAddr = get_addr(RA),
    RPort = get_port(RP),
    proc_lib:init_ack(init_rc(LAddrOpt)),
    Sock = ok(connect(Mod, RAddr, RPort, gen_opts(LAddrOpt, Rest))),
    publish(Mod, T, Ref, Sock),
    up(Pid, {RAddr, RPort}, LAddrOpt, Mod, Sock),
    Sock.

init_rc([{ip, Addr}]) ->
    {ok, self(), [Addr]};
init_rc([]) ->
    {ok, self()}.

up(Pid, Remote, [{ip, _Addr}], _, _) ->
    diameter_peer:up(Pid, Remote);
up(Pid, Remote, [], Mod, Sock) ->
    {Addr, _Port} = ok(sockname(Mod, Sock)),
    diameter_peer:up(Pid, Remote, [Addr]).

publish(Mod, T, Ref, Sock) ->
    true = diameter_reg:add_new({?MODULE, T, {Ref, Sock}}),
    putr(?INFO_KEY, {Mod, Sock}).  %% for info/1

ok({ok, T}) ->
    T;
ok(No) ->
    x(No).

x(Reason) ->
    exit({shutdown, Reason}).

%% accept_peer/3

accept_peer(_Mod, _Sock, []) ->
    ok;

accept_peer(Mod, Sock, Matches) ->
    {RAddr, _} = ok(peername(Mod, Sock)),
    diameter_peer:match([RAddr], Matches)
        orelse x({accept, RAddr, Matches}),
    ok.

%% accept/1

accept(Opts) ->
    [[M] || {accept, M} <- Opts].

%% listener/2

%% Accepting processes can be started concurrently: ensure only one
%% listener is started.
listener(LRef, T) ->
    diameter_sync:call({?MODULE, listener, LRef},
                       {?MODULE, listener, [{LRef, T, self()}]},
                       infinity,
                       infinity).

listener({LRef, T, TPid}) ->
    l(diameter_reg:match({?MODULE, listener, {LRef, '_'}}), LRef, T, TPid).

%% Existing listening process ...
l([{{?MODULE, listener, {_, AS}}, LPid}], _, _, TPid) ->
    LPid ! {accept, TPid},
    AS;

%% ... or not.
l([], LRef, T, TPid) ->
    {ok, _, AS} = diameter_tcp_sup:start_child({listen, LRef, TPid, T}),
    AS.

%% get_addr/1

get_addr(As) ->
    diameter_lib:ipaddr(addr(As, [])).

%% get_addr/2

get_addr([], []) ->
    [];
get_addr(As, Def) ->
    [{ip, diameter_lib:ipaddr(addr(As, Def))}].

%% Take the first address from the service if several are unspecified.
addr([], [Addr | _]) ->
    Addr;
addr([{_, Addr}], _) ->
    Addr;
addr(As, Addrs) ->
    ?ERROR({invalid_addrs, As, Addrs}).

%% get_port/1

get_port([{_, Port}]) ->
    Port;
get_port([]) ->
    ?DEFAULT_PORT;
get_port(Ps) ->
    ?ERROR({invalid_ports, Ps}).

%% gen_opts/2

gen_opts(LAddrOpt, Opts) ->
    {L,_} = proplists:split(Opts, [binary, packet, active]),
    [[],[],[]] == L orelse ?ERROR({reserved_options, Opts}),
    [binary, {packet, 0}, {active, false}] ++ LAddrOpt ++ Opts.

%% ---------------------------------------------------------------------------
%% # ports/1
%% ---------------------------------------------------------------------------

ports() ->
    Ts = diameter_reg:match({?MODULE, '_', '_'}),
    [{type(T), resolve(T,S), Pid} || {{?MODULE, T, {_,S}}, Pid} <- Ts].

ports(Ref) ->
    Ts = diameter_reg:match({?MODULE, '_', {Ref, '_'}}),
    [{type(T), resolve(T,S), Pid} || {{?MODULE, T, {R,S}}, Pid} <- Ts,
                                     R == Ref].

type(listener) ->
    listen;
type(T) ->
    T.

sock(listener, {_LAddr, Sock}) ->
    Sock;
sock(_, Sock) ->
    Sock.

resolve(Type, S) ->
    Sock = sock(Type, S),
    try
        ok(portnr(Sock))
    catch
        _:_ -> Sock
    end.

portnr(Sock)
  when is_port(Sock) ->
    portnr(gen_tcp, Sock);
portnr(Sock) ->
    portnr(ssl, Sock).

%% ---------------------------------------------------------------------------
%% # handle_call/3
%% ---------------------------------------------------------------------------

handle_call(_, _, State) ->
    {reply, nok, State}.

%% ---------------------------------------------------------------------------
%% # handle_cast/2
%% ---------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%% ---------------------------------------------------------------------------
%% # handle_info/2
%% ---------------------------------------------------------------------------

handle_info(T, #transport{} = S) ->
    {noreply, #transport{} = t(T,S)};

handle_info(T, #listener{} = S) ->
    {noreply, #listener{} = l(T,S)};

handle_info(T, #monitor{} = S) ->
    m(T,S),
    x(T).

%% ---------------------------------------------------------------------------
%% # code_change/3
%% ---------------------------------------------------------------------------

code_change(_, State, _) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% # terminate/2
%% ---------------------------------------------------------------------------

terminate(_, _) ->
    ok.

%% ---------------------------------------------------------------------------

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).

%% m/2
%%
%% Transition monitor state.

%% Transport is telling us to die.
m({stop, TPid}, #monitor{transport = TPid}) ->
    ok;

%% Transport has died.
m({'DOWN', _, process, TPid, _}, #monitor{transport = TPid}) ->
    ok;

%% Transport parent has died.
m({'DOWN', _, process, Pid, _}, #monitor{parent = Pid,
                                         transport = TPid}) ->
    exit(TPid, {shutdown, parent}).

%% l/2
%%
%% Transition listener state.

%% An accepting transport is attaching.
l({accept, TPid}, #listener{count = N} = S) ->
    monitor(process, TPid),
    S#listener{count = N+1};

%% Accepting process has died.
l({'DOWN', _, process, _, _}, #listener{count = N} = S) ->
    S#listener{count = N-1};

%% Transport has been removed.
l({transport, remove, _} = T, #listener{socket = Sock}) ->
    gen_tcp:close(Sock),
    x(T).

%% t/2
%%
%% Transition transport state.

t(T,S) ->
    case transition(T,S) of
        ok ->
            S;
        #transport{} = NS ->
            NS;
        stop ->
            x(T)
    end.

%% transition/2

%% Incoming message.
transition({P, Sock, Bin}, #transport{socket = Sock,
                                      ssl = B,
                                      throttled = T}
                           = S)
  when P == ssl, true == B;
       P == tcp ->
    false = T,  %% assert
    recv(Bin, S);

%% Make a new throttling callback after a timeout.
transition(throttle, #transport{throttled = false}) ->
    ok;
transition(throttle, S) ->
    throttle(S);

%% Capabilties exchange has decided on whether or not to run over TLS.
transition({diameter, {tls, Ref, Type, B}}, #transport{parent = Pid}
                                            = S) ->
    true = is_boolean(B),  %% assert
    #transport{}
        = NS
        = tls_handshake(Type, B, S),
    Pid ! {diameter, {tls, Ref}},
    throttle(NS#transport{ssl = B});

transition({C, Sock}, #transport{socket = Sock,
                                 ssl = B})
  when C == tcp_closed, not B;
       C == ssl_closed, B ->
    stop;

transition({E, Sock, _Reason} = T, #transport{socket = Sock,
                                              ssl = B}
                                   = S)
  when E == tcp_error, not B;
       E == ssl_error, B ->
    ?ERROR({T,S});

%% Outgoing message.
transition({diameter, {send, Bin}}, S) ->
    send(Bin, S);

%% Request to close the transport connection.
transition({diameter, {close, Pid}}, #transport{parent = Pid,
                                                socket = Sock,
                                                module = M}) ->
    M:close(Sock),
    stop;

%% Timeout for reception of outstanding packets.
transition({timeout, TRef, flush}, #transport{tref = TRef} = S) ->
    flush(S#transport{tref = false});

%% Request for the local port number.
transition({resolve_port, Pid}, #transport{socket = Sock,
                                           module = M})
  when is_pid(Pid) ->
    Pid ! portnr(M, Sock),
    ok;

%% Parent process has died.
transition({'DOWN', _, process, Pid, _}, #transport{parent = Pid}) ->
    stop.

%% Crash on anything unexpected.

%% tls_handshake/3
%%
%% In the case that no tls message is received (eg. the service hasn't
%% been configured to advertise TLS support) we will simply never ask
%% for another TCP message, which will force the watchdog to
%% eventually take us down.

%% TLS has already been established with the connection.
tls_handshake(_, _, #transport{ssl = true} = S) ->
    S;

%% Capabilities exchange negotiated TLS but transport was not
%% configured with an options list.
tls_handshake(_, true, #transport{ssl = false}) ->
    ?ERROR(no_ssl_options);

%% Capabilities exchange negotiated TLS: upgrade the connection.
tls_handshake(Type, true, #transport{socket = Sock,
                                     module = M,
                                     ssl = Opts}
                          = S) ->
    {ok, SSock} = tls(Type, Sock, [{cb_info, ?TCP_CB(M)} | Opts]),
    Ref = getr(?REF_KEY),
    true = diameter_reg:add_new({?MODULE, Type, {Ref, SSock}}),
    S#transport{socket = SSock,
                module = ssl};

%% Capabilities exchange has not negotiated TLS.
tls_handshake(_, false, S) ->
    S.

tls(connect, Sock, Opts) ->
    ssl:connect(Sock, Opts);
tls(accept, Sock, Opts) ->
    ssl:ssl_accept(Sock, Opts).

%% recv/2
%%
%% Reassemble fragmented messages and extract multiple message sent
%% using Nagle.

%% Receive packets until a full message is received,
recv(Bin, #transport{frag = Head, throttled = false} = S) ->
    case rcv(Head, Bin) of
        {Msg, B} ->
            throttle(S#transport{frag = B, throttled = Msg});
        Frag ->
            setopts(S),
            start_fragment_timer(S#transport{frag = Frag,
                                             flush = false})
    end.

%% recv/1

recv(#transport{throttled = false} = S) ->
    recv(<<>>, S);

recv(#transport{} = S) ->
    S.

%% rcv/2

%% No previous fragment.
rcv(<<>>, Bin) ->
    rcv(Bin);

%% Not even the first four bytes of the header.
rcv(Head, Bin)
  when is_binary(Head) ->
    rcv(<<Head/binary, Bin/binary>>);

%% Or enough to know how many bytes to extract.
rcv({Len, N, Head, Acc}, Bin) ->
    rcv(Len, N + size(Bin), Head, [Bin | Acc]).

%% rcv/4

%% Extract a message for which we have all bytes.
rcv(Len, N, Head, Acc)
  when Len =< N ->
    recv1(Len, bin(Head, Acc));

%% Wait for more packets.
rcv(Len, N, Head, Acc) ->
    {Len, N, Head, Acc}.

%% rcv/1

%% Nothing left.
rcv(<<>> = Bin) ->
    Bin;

%% The Message Length isn't even sufficient for a header. Chances are
%% things will go south from here but if we're lucky then the bytes we
%% have extend to an intended message boundary and we can recover by
%% simply receiving them. Make it so.
rcv(<<_:1/binary, Len:24, _/binary>> = Bin)
  when Len < 20 ->
    {Bin, <<>>};

%% Enough bytes to extract a message.
rcv(<<_:1/binary, Len:24, _/binary>> = Bin)
  when Len =< size(Bin) ->
    recv1(Len, Bin);

%% Or not: wait for more packets.
rcv(<<_:1/binary, Len:24, _/binary>> = Head) ->
    {Len, size(Head), Head, []};

%% Not even 4 bytes yet.
rcv(Head) ->
    Head.

%% recv1/2

recv1(Len, Bin) ->
    <<Msg:Len/binary, Rest/binary>> = Bin,
    {Msg, Rest}.

%% bin/1-2

bin(Head, Acc) ->
    list_to_binary([Head | lists:reverse(Acc)]).

bin({_, _, Head, Acc}) ->
    bin(Head, Acc);
bin(Bin)
  when is_binary(Bin) ->
    Bin.

%% flush/1

%% An erroneously large message length may leave us with a fragment
%% that lingers if the peer doesn't have anything more to send. Start
%% a timer to force reception if an incoming message doesn't arrive
%% first. This won't stop a peer from sending a large bogus value and
%% following it up however but such a state of affairs can only go on
%% for so long since an unanswered DWR will eventually be the result.
%%
%% An erroneously small message length causes problems as well but
%% since all messages with length problems are discarded this should
%% also eventually lead to watchdog failover.

%% No fragment to flush or not receiving messages.
flush(#transport{frag = Frag, throttled = B} = S)
  when Frag == <<>>;
       B /= false ->
    S;

%% Messages have been received since last timer expiry.
flush(#transport{flush = false} = S) ->
    start_fragment_timer(S#transport{flush = true});

%% No messages since last expiry.
flush(#transport{frag = Frag, parent = Pid} = S) ->
    diameter_peer:recv(Pid, bin(Frag)),
    S#transport{frag = <<>>}.

%% start_fragment_timer/1
%%
%% Start a timer only if there's none running and a message to flush.

start_fragment_timer(#transport{frag = B, tref = TRef} = S)
  when B == <<>>;
       TRef /= false ->
    S;

start_fragment_timer(#transport{timeout = Tmo} = S) ->
    S#transport{tref = erlang:start_timer(Tmo, self(), flush)}.

%% accept/2

accept(ssl, LSock) ->
    case ssl:transport_accept(LSock) of
        {ok, Sock} ->
            {ssl:ssl_accept(Sock), Sock};
        {error, _} = No ->
            No
    end;
accept(Mod, LSock) ->
    Mod:accept(LSock).

%% connect/4

connect(Mod, Host, Port, Opts) ->
    Mod:connect(Host, Port, Opts).

%% send/2

send(Bin, #transport{socket = Sock,
                     module = M}) ->
    case send(M, Sock, Bin) of
        ok ->
            ok;
        {error, Reason} ->
            x({send, Reason})
    end.

%% send/3

send(gen_tcp, Sock, Bin) ->
    gen_tcp:send(Sock, Bin);
send(ssl, Sock, Bin) ->
    ssl:send(Sock, Bin);
send(M, Sock, Bin) ->
    M:send(Sock, Bin).

%% setopts/3

setopts(gen_tcp, Sock, Opts) ->
    inet:setopts(Sock, Opts);
setopts(ssl, Sock, Opts) ->
    ssl:setopts(Sock, Opts);
setopts(M, Sock, Opts) ->
    M:setopts(Sock, Opts).

%% setopts/1

setopts(#transport{socket = Sock, module = M}) ->
    setopts(M, Sock).

%% setopts/2

setopts(M, Sock) ->
    case setopts(M, Sock, [{active, once}]) of
        ok -> ok;
        X  -> x({setopts, M, Sock, X})  %% possibly on peer disconnect
    end.

%% throttle/1

%% Still collecting packets for a complete message: keep receiving.
throttle(#transport{throttled = false} = S) ->
    recv(S);

%% Decide whether to receive another, or whether to accept a message
%% that's been received.
throttle(#transport{throttle_cb = F, throttled = T} = S) ->
    Res = cb(F, T),

    try throttle(Res, S) of
        #transport{ssl = SB} = NS when is_boolean(SB) ->
            throttle(defrag(NS));
        #transport{throttled = Msg} = NS when is_binary(Msg) ->
            %% Initial incoming message when we might need to upgrade
            %% to TLS: wait for reception of a tls tuple.
            defrag(NS)
    catch
        #transport{} = NS ->
            recv(NS)
    end.

%% cb/2

cb(false, _) ->
    ok;

cb(F, B) ->
    diameter_lib:eval([F, true /= B andalso B]).

%% throttle/2

%% Callback says to receive another message.
throttle(ok, #transport{throttled = true} = S) ->
    throw(S#transport{throttled = false});

%% Callback says to accept a received message.
throttle(ok, #transport{parent = Pid, throttled = Msg} = S)
  when is_binary(Msg) ->
    diameter_peer:recv(Pid, Msg),
    S;

throttle({ok = T, F}, S) ->
    throttle(T, S#transport{throttle_cb = F});

%% Callback says to accept a received message and acknowledged the
%% returned pid with a {request, Pid} message if a request pid is
%% spawned, a discard message otherwise. The latter does not mean that
%% the message was necessarily discarded: it could have been an
%% answer.
throttle(NPid, #transport{parent = Pid, throttled = Msg} = S)
  when is_pid(NPid), is_binary(Msg) ->
    diameter_peer:recv(Pid, {Msg, NPid}),
    S;

throttle({NPid, F}, #transport{throttled = Msg} = S)
  when is_pid(NPid), is_binary(Msg) ->
    throttle(NPid, S#transport{throttle_cb = F});

%% Callback to accept a received message says to discard it.
throttle(discard, #transport{throttled = Msg} = S)
  when is_binary(Msg) ->
    S;

throttle({discard = T, F}, #transport{throttled = Msg} = S)
  when is_binary(Msg) ->
    throttle(T, S#transport{throttle_cb = F});

%% Callback to accept a received message says to answer it with the
%% supplied binary.
throttle(Bin, #transport{throttled = Msg} = S)
  when is_binary(Bin), is_binary(Msg) ->
    send(Bin, S),
    S;

throttle({Bin, F}, #transport{throttled = Msg} = S)
  when is_binary(Bin), is_binary(Msg) ->
    throttle(Bin, S#transport{throttle_cb = F});

%% Callback says to ask again in the specified number of milliseconds.
throttle({timeout, Tmo}, S) ->
    erlang:send_after(Tmo, self(), throttle),
    throw(S);

throttle({timeout = T, Tmo, F}, S) ->
    throttle({T, Tmo}, S#transport{throttle_cb = F});

throttle(T, #transport{throttle_cb = F}) ->
    ?ERROR({invalid_return, T, F}).

%% defrag/1
%%
%% Try to extract another message from packets already read before
%% another throttling callback.

defrag(#transport{frag = Head} = S) ->
    case rcv(Head, <<>>) of
        {Msg, B} ->
            S#transport{throttled = Msg, frag = B};
        _ ->
            S#transport{throttled = true}
    end.

%% portnr/2

portnr(gen_tcp, Sock) ->
    inet:port(Sock);
portnr(M, Sock) ->
    case M:sockname(Sock) of
        {ok, {_Addr, PortNr}} ->
            {ok, PortNr};
        {error, _} = No ->
            No
    end.

%% sockname/2

sockname(gen_tcp, Sock) ->
    inet:sockname(Sock);
sockname(M, Sock) ->
    M:sockname(Sock).

%% peername/2

peername(gen_tcp, Sock) ->
    inet:peername(Sock);
peername(M, Sock) ->
    M:peername(Sock).

%% getstat/2

getstat(gen_tcp, Sock) ->
    inet:getstat(Sock);
getstat(M, Sock) ->
    M:getstat(Sock).
%% Note that ssl:getstat/1 doesn't yet exist in R15B01.
