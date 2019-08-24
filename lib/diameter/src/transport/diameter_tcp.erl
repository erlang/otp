%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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
-define(TRANSPORT_KEY, transport).

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
%% a process owning the listening port. The monitor process
%% historically died after connection establishment, but can now live
%% on as the sender of outgoing messages, so that a blocking send
%% doesn't prevent messages from being received.

%% Listener process state.
-record(listener, {socket :: inet:socket(),
                   module :: module(),
                   service = false :: false | pid()}). %% service process

%% Monitor process state.
-record(monitor,
        {parent :: reference() | false | pid(),
         transport = self() :: pid(),
         ack = false :: boolean(),
         socket :: inet:socket() | ssl:sslsocket() | undefined,
         module :: module() | undefined}).

-type length() :: 0..16#FFFFFF. %% message length from Diameter header
-type frag()   :: maybe_improper_list(length(), binary())
                | binary().

-type connect_option() :: {raddr, inet:ip_address()}
                        | {rport, pos_integer()}
                        | {ssl_options, true | [ssl:tls_client_option()]}
                        | option()
                        | ssl:tls_client_option()
                        | gen_tcp:connect_option().

-type match() :: inet:ip_address()
               | string()
               | [match()].

-type listen_option() :: {accept, match()}
                       | {ssl_options, true | [ssl:tls_server_option()]}
                       | option()
                       | ssl:tls_server_option()
                       | gen_tcp:listen_option().

-type option() :: {port, non_neg_integer()}
                | {sender, boolean()}
                | sender
                | {message_cb, false | diameter:eval()}
                | {fragment_timer, 0..16#FFFFFFFF}.

%% Accepting/connecting transport process state.
-record(transport,
        {socket  :: inet:socket() | ssl:sslsocket(), %% accept/connect socket
         active = false :: boolean(),           %% is socket active?
         recv   = true  :: boolean(),           %% should it be active?
         parent  :: pid(),          %% of process that started us
         module  :: module(),       %% gen_tcp-like module
         ssl     :: [term()] | boolean(),       %% ssl options, ssl or not
         frag = <<>> :: frag(),                 %% message fragment
         timeout :: infinity | 0..16#FFFFFFFF,  %% fragment timeout
         tref = false  :: false | reference(),  %% fragment timer reference
         flush = false :: boolean(),            %% flush fragment at timeout?
         message_cb  :: false | diameter:eval(),
         send        :: pid() | false}).         %% sending process

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
   -> {ok, pid()}
 when Ref :: diameter:transport_ref().

start({T, Ref}, Svc, Opts) ->
    #diameter_service{capabilities = Caps,
                      pid = SvcPid}
        = Svc,

    diameter_tcp_sup:start(),  %% start tcp supervisors on demand
    {Mod, Rest} = split(Opts),
    Addrs = Caps#diameter_caps.host_ip_address,
    Arg = {T, Ref, Mod, self(), Rest, Addrs, SvcPid},
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
i({T, Ref, Mod, Pid, Opts, Addrs, SvcPid})
  when T == accept;
       T == connect ->
    monitor(process, Pid),
    %% Since accept/connect might block indefinitely, spawn a process
    %% that kills us with the parent until call returns, and then
    %% sends outgoing messages.
    {[SO|TO], Rest} = proplists:split(Opts, [ssl_options,
                                             sender,
                                             message_cb,
                                             fragment_timer]),
    SslOpts = ssl_opts(SO),
    OwnOpts = lists:append(TO),
    Tmo = proplists:get_value(fragment_timer,
                              OwnOpts,
                              ?DEFAULT_FRAGMENT_TIMEOUT),
    [CB, Sender] = [proplists:get_value(K, OwnOpts, false)
                    || K <- [message_cb, sender]],
    ?IS_TIMEOUT(Tmo) orelse ?ERROR({fragment_timer, Tmo}),
    {ok, MPid} = diameter_tcp_sup:start_child(#monitor{parent = Pid}),
    Sock = init(T, Ref, Mod, Pid, SslOpts, Rest, Addrs, SvcPid),
    M = if SslOpts -> ssl; true -> Mod end,
    Sender andalso monitor(process, MPid),
    false == CB orelse (Pid ! {diameter, ack}),
    MPid ! {start, self(), Sender andalso {Sock, M}, false /= CB},
    putr(?REF_KEY, Ref),
    setopts(#transport{parent = Pid,
                       module = M,
                       socket = Sock,
                       ssl = SslOpts,
                       message_cb = CB,
                       timeout = Tmo,
                       send = Sender andalso MPid});
%% Put the reference in the process dictionary since we now use it
%% advertise the ssl socket after TLS upgrade.

%% A monitor process to kill the transport if the parent dies.
i(#monitor{parent = Pid, transport = TPid} = S) ->
    putr(?TRANSPORT_KEY, TPid),
    proc_lib:init_ack({ok, self()}),
    monitor(process, TPid),
    S#monitor{parent = monitor(process, Pid)};
%% In principle a link between the transport and killer processes
%% could do the same thing: have the accepting/connecting process be
%% killed when the killer process dies as a consequence of parent
%% death. However, a link can be unlinked and this is exactly what
%% gen_tcp seems to do. Links should be left to supervisors.

i({listen, Ref, {Mod, Opts, Addrs}}) ->
    [_] = diameter_config:subscribe(Ref, transport), %% assert existence
    {[LP], Rest} = proplists:split(Opts, [port]),
    {ok, LSock} = Mod:listen(get_port(LP), gen_opts(Addrs, Rest)),
    {ok, {LAddr, _}} = sockname(Mod, LSock),
    true = diameter_reg:add_new({?MODULE, listener, {Ref, {LAddr, LSock}}}),
    proc_lib:init_ack({ok, self(), {LAddr, LSock}}),
    #listener{socket = LSock,
              module = Mod}.

ssl_opts([]) ->
    false;
ssl_opts([{ssl_options, true}]) ->
    true;
ssl_opts([{ssl_options, Opts}])
  when is_list(Opts) ->
    Opts;
ssl_opts(T) ->
    ?ERROR({ssl_options, T}).

%% init/8

%% Establish a TLS connection before capabilities exchange ...
init(Type, Ref, Mod, Pid, true, Opts, Addrs, SvcPid) ->
    init(Type, Ref, ssl, Pid, [{cb_info, ?TCP_CB(Mod)} | Opts], Addrs, SvcPid);

%% ... or not.
init(Type, Ref, Mod, Pid, _, Opts, Addrs, SvcPid) ->
    init(Type, Ref, Mod, Pid, Opts, Addrs, SvcPid).

%% init/7

init(accept = T, Ref, Mod, Pid, Opts, Addrs, SvcPid) ->
    {[Matches], Rest} = proplists:split(Opts, [accept]),
    {ok, LPid, {LAddr, LSock}} = listener(Ref, {Mod, Rest, Addrs}),
    ok = gen_server:call(LPid, {accept, SvcPid}, infinity),
    proc_lib:init_ack({ok, self(), [LAddr]}),
    Sock = ok(accept(Mod, LSock)),
    ok = accept_peer(Mod, Sock, accept(Matches)),
    publish(Mod, T, Ref, Sock),
    diameter_peer:up(Pid),
    Sock;

init(connect = T, Ref, Mod, Pid, Opts, Addrs, _SvcPid) ->
    {[RA, RP], Rest} = proplists:split(Opts, [raddr, rport]),
    RAddr = get_addr(RA),
    RPort = get_port(RP),
    proc_lib:init_ack({ok, self()}),
    Sock = ok(connect(Mod, RAddr, RPort, gen_opts(Addrs, Rest))),
    publish(Mod, T, Ref, Sock),
    up(Pid, {RAddr, RPort}, Mod, Sock),
    Sock.

up(Pid, Remote, Mod, Sock) ->
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
listener(Ref, T) ->
    diameter_sync:call({?MODULE, listener, Ref},
                       {?MODULE, listener, [{Ref, T, self()}]},
                       infinity,
                       infinity).

%% listener/1

listener({Ref, T, _TPid}) ->
    l(diameter_reg:match({?MODULE, listener, {Ref, '_'}}), Ref, T).

%% l/3

%% Existing listening process ...
l([{{?MODULE, listener, {_, AS}}, LPid}], _, _) ->
    {ok, LPid, AS};

%% ... or not.
l([], Ref, T) ->
    diameter_tcp_sup:start_child({listen, Ref, T}).

%% addrs/2
%%
%% Take the first address from the service if several are specified
%% and not address is configured.

addrs(Addrs, Opts) ->
    case lists:mapfoldr(fun ipaddr/2, [], Opts) of
        {Os, [_]} ->
            Os;
        {_, []} ->
            Opts ++ [{ip, A} || [A|_] <- [Addrs]];
        {_, As} ->
            ?ERROR({invalid_addrs, As, Addrs})
    end.

ipaddr({K,A}, As)
  when K == ifaddr;
       K == ip ->
    {{ip, ipaddr(A)}, [A | As]};
ipaddr(T, B) ->
    {T, B}.

ipaddr(A)
  when A == loopback;
       A == any ->
    A;
ipaddr(A) ->
    diameter_lib:ipaddr(A).

%% get_addr/1

get_addr([{_, Addr}]) ->
    diameter_lib:ipaddr(Addr);
get_addr(Addrs) ->
    ?ERROR({invalid_addrs, Addrs}).

%% get_port/1

get_port([{_, Port}]) ->
    Port;
get_port([]) ->
    ?DEFAULT_PORT;
get_port(Ps) ->
    ?ERROR({invalid_ports, Ps}).

%% gen_opts/2

gen_opts(Addrs, Opts) ->
    gen_opts(addrs(Addrs, Opts)).

%% gen_opts/1

gen_opts(Opts) ->
    {L,_} = proplists:split(Opts, [binary, packet, active]),
    [[],[],[]] == L orelse ?ERROR({reserved_options, Opts}),
    [binary, {packet, 0}, {active, false} | Opts].

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

handle_call({accept, SvcPid}, _From, #listener{service = P} = S) ->
    {reply, ok, if not is_pid(P), is_pid(SvcPid) ->
                        monitor(process, SvcPid),
                        S#listener{service = SvcPid};
                   true ->
                        S
                end};

%% Transport is telling us of parent death.
handle_call({stop, _Pid} = Reason, _From, #monitor{} = S) ->
    {stop, {shutdown, Reason}, ok, S};

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
    {noreply, #monitor{} = m(T,S)}.

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

%% Outgoing message.
m(Msg, S)
  when is_record(Msg, diameter_packet);
       is_binary(Msg) ->
    send(Msg, S),
    S;

%% Transport has established a connection. Stop monitoring on the
%% parent so as not to die before a send from the transport.
m({start, TPid, T, Ack} = M, #monitor{transport = TPid} = S) ->
    case T of
        {Sock, Mod} ->
            demonitor(S#monitor.parent, [flush]),
            S#monitor{parent = false,
                      socket = Sock,
                      module = Mod,
                      ack = Ack};
        false ->  %% monitor not sending
            x(M)
    end;

%% Transport is telling us to die.
m({stop, TPid} = T, #monitor{transport = TPid}) ->
    x(T);

%% Transport is telling us to die.
m({stop, TPid} = T, #monitor{transport = TPid}) ->
    x(T);

%% Transport is telling us that TLS has been negotiated after
%% capabilities exchange.
m({tls, SSock}, S) ->
    S#monitor{socket = SSock,
              module = ssl};

%% Transport or parent has died.
m({'DOWN', M, process, P, _} = T, #monitor{parent = MRef,
                                           transport = TPid})
  when M == MRef;
       P == TPid ->
    x(T).

%% l/2
%%
%% Transition listener state.

%% Service process has died.
l({'DOWN', _, process, Pid, _} = T, #listener{service = Pid,
                                              socket = Sock,
                                              module = M}) ->
    M:close(Sock),
    x(T);

%% Transport has been removed.
l({transport, remove, _} = T, #listener{socket = Sock,
                                        module = M}) ->
    M:close(Sock),
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

%% Incoming packets.
transition({P, Sock, Bin}, #transport{socket = Sock,
                                      ssl = B,
                                      frag = Frag}
                           = S)
  when P == ssl, true == B;
       P == tcp ->
    recv(acc(Frag, Bin), S);

%% Capabilties exchange has decided on whether or not to run over TLS.
transition({diameter, {tls, Ref, Type, B}}, #transport{parent = Pid}
                                            = S) ->
    true = is_boolean(B),  %% assert
    #transport{}
        = NS
        = tls_handshake(Type, B, S),
    Pid ! {diameter, {tls, Ref}},
    NS#transport{ssl = B};

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
transition({diameter, {send, Msg}}, #transport{} = S) ->
    message(send, Msg, S);

%% Monitor has sent an outgoing message.
transition(Msg, S)
  when is_record(Msg, diameter_packet);
       is_binary(Msg) ->
    message(ack, Msg, S);

%% Deferred actions from a message_cb.
transition({actions, Dir, Acts}, S) ->
    setopts(actions(Acts, Dir, S));

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

%% Parent process has died: call the monitor to not close the socket
%% during an ongoing send, but don't let it take forever.
transition({'DOWN', _, process, Pid, _}, #transport{parent = Pid,
                                                    send = MPid}) ->
    false == MPid
        orelse (ok == gen_server:call(MPid, {stop, self()}, 1000))
        orelse exit(MPid, {shutdown, parent}),
    stop;

%% Monitor process has died.
transition({'DOWN', _, process, MPid, _}, #transport{send = MPid})
  when is_pid(MPid) ->
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
                                     ssl = Opts,
                                     send = MPid}
                          = S) ->
    {ok, SSock} = tls(Type, Sock, [{cb_info, ?TCP_CB(M)} | Opts]),
    Ref = getr(?REF_KEY),
    true = diameter_reg:add_new({?MODULE, Type, {Ref, SSock}}),
    false == MPid orelse (MPid ! {tls, SSock}), %% tell the sender process
    S#transport{socket = SSock,
                module = ssl};

%% Capabilities exchange has not negotiated TLS.
tls_handshake(_, false, S) ->
    S.

tls(connect, Sock, Opts) ->
    ssl:connect(Sock, Opts);
tls(accept, Sock, Opts) ->
    ssl:handshake(Sock, Opts).  %% assume no handshake option

%% recv/2
%%
%% Reassemble fragmented messages and extract multiple message sent
%% using Nagle.

%% Receive packets until a full message is received,

recv({Msg, Rest}, S) ->  %% have a complete message ...
    recv(acc(Rest), message(recv, Msg, S));

recv(Frag, #transport{recv = B,
                      socket = Sock,
                      module = M}
           = S) ->       %% or not
    B andalso setopts(M, Sock),
    start_fragment_timer(S#transport{frag = Frag,
                                     flush = false,
                                     active = B}).

%% acc/2

%% Know how many bytes to extract.
acc([Len | Acc], Bin) ->
    acc1(Len, <<Acc/binary, Bin/binary>>);

%% Or not.
acc(Head, Bin) ->
    acc(<<Head/binary, Bin/binary>>).

%% acc1/3

%% Extract a message for which we have all bytes.
acc1(Len, Bin)
  when Len =< byte_size(Bin) ->
    split_binary(Bin, Len);

%% Wait for more packets.
acc1(Len, Bin) ->
    [Len | Bin].

%% acc/1

%% Don't match on Bin since this results in it being copied at the
%% next append according to the Efficiency Guide. This is also the
%% reason that the Len is extracted and maintained when accumulating
%% messages. The simplest implementation is just to accumulate a
%% binary and match <<_, Len:24, _/binary>> each time the length is
%% required, but the performance of this decays quadratically with the
%% message length, since the binary is then copied with each append of
%% additional bytes from gen_tcp.

acc(Bin)
  when 3 < byte_size(Bin) ->
    {Head, _} = split_binary(Bin, 4),
    [_,A,B,C] = binary_to_list(Head),
    Len = (A bsl 16) bor (B bsl 8) bor C,
    if Len < 20 ->
            %% Message length isn't sufficient for a Diameter Header.
            %% Chances are things will go south from here but if we're
            %% lucky then the bytes we have extend to an intended
            %% message boundary and we can recover by simply receiving
            %% them. Make it so.
            {Bin, <<>>};
       true ->
            acc1(Len, Bin)
    end;

%% Not even 4 bytes yet.
acc(Bin) ->
    Bin.

%% bin/1

bin([_ | Bin]) ->
    Bin;

bin(Bin) ->
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
flush(#transport{frag = <<>>} = S) ->
    S;

%% Messages have been received since last timer expiry.
flush(#transport{flush = false} = S) ->
    start_fragment_timer(S#transport{flush = true});

%% No messages since last expiry.
flush(#transport{frag = Frag} = S) ->
    message(recv, bin(Frag), S#transport{frag = <<>>}).

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
            ssl:handshake(Sock);
        {error, _} = No ->
            No
    end;
accept(Mod, LSock) ->
    Mod:accept(LSock).

%% connect/4

connect(Mod, Host, Port, Opts) ->
    Mod:connect(Host, Port, Opts).

%% send/2

send(Msg, #monitor{socket = Sock, module = M, transport = TPid, ack = B}) ->
    send1(M, Sock, Msg),
    B andalso (TPid ! Msg);

send(Msg, #transport{socket = Sock, module = M, send = false} = S) ->
    send1(M, Sock, Msg),
    message(ack, Msg, S);

%% Send from the monitor process to avoid deadlock if both the
%% receiver and the peer were to block in send.
send(Msg, #transport{send = Pid} = S) ->
    Pid ! Msg,
    S.

%% send1/3

send1(Mod, Sock, #diameter_packet{bin = Bin}) ->
    send1(Mod, Sock, Bin);

send1(Mod, Sock, Bin) ->
    case send(Mod, Sock, Bin) of
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

setopts(#transport{socket = Sock,
                   active = A,
                   recv = B,
                   module = M}
        = S)
  when B, not A ->
    setopts(M, Sock),
    S#transport{active = true};

setopts(S) ->
    S.

%% setopts/2

setopts(M, Sock) ->
    case setopts(M, Sock, [{active, once}]) of
        ok -> ok;
        X  -> x({setopts, Sock, M, X})  %% possibly on peer disconnect
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

%% A message_cb is invoked whenever a message is sent or received, or
%% to provide acknowledgement of a completed send or discarded
%% request. Ignoring possible extra arguments, calls are of the
%% following form.
%%
%% cb(recv, Msg)          Receive a message into diameter?
%% cb(send, Msg)          Send a message on the socket?
%% cb(ack,  Msg)          Acknowledgement of a completed send.
%% cb(ack,  false)        Acknowledgement of a discarded request.
%%
%% Msg will be binary() in a recv callback, but can be a
%% diameter_packet record in a send/ack callback if a recv/send
%% callback returns a record. Callbacks return a list of the following
%% form.
%%
%%   [boolean() | send | recv | binary() | #diameter_packet{}]
%%
%% The atoms are meaningless by themselves, but say whether subsequent
%% messages are to be sent or received. A boolean says whether or not
%% to continue reading on the socket. Messages can be received even
%% after false is returned if these arrived in the same packet. A
%% leading recv or send is implicit on the corresponding callbacks. A
%% new callback can be returned as the tail of a returned list: any
%% value not of the aforementioned list type is interpreted as a
%% callback.

%% message/3

message(send, false = M, S) ->
    message(ack, M, S);

message(ack, _, #transport{message_cb = false} = S) ->
    S;

message(Dir, Msg, #transport{message_cb = CB} = S) ->
    setopts(actions(cb(CB, Dir, Msg), Dir, S)).

%% actions/3

actions([], _, S) ->
    S;

actions([B | As], Dir, S)
  when is_boolean(B) ->
    actions(As, Dir, S#transport{recv = B});

actions([Dir | As], _, S)
  when Dir == send;
       Dir == recv ->
    actions(As, Dir, S);

actions([Msg | As], send = Dir, S)
  when is_binary(Msg);
       is_record(Msg, diameter_packet) ->
    actions(As, Dir, send(Msg, S));

actions([Msg | As], recv = Dir, #transport{parent = Pid} = S)
  when is_binary(Msg);
       is_record(Msg, diameter_packet) ->
    diameter_peer:recv(Pid, Msg),
    actions(As, Dir, S);

actions([{defer, Tmo, Acts} | As], Dir, S) ->
    erlang:send_after(Tmo, self(), {actions, Dir, Acts}),
    actions(As, Dir, S);

actions(CB, _, S) ->
    S#transport{message_cb = CB}.

%% cb/3

cb(false, _, Msg) ->
    [Msg];

cb(CB, Dir, Msg) ->
    diameter_lib:eval([CB, Dir, Msg]).
