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

-include_lib("diameter/include/diameter.hrl").

-define(ERROR(T), erlang:error({T, ?MODULE, ?LINE})).

-define(DEFAULT_PORT, 3868).  %% RFC 3588, ch 2.1
-define(LISTENER_TIMEOUT, 30000).
-define(FRAGMENT_TIMEOUT, 1000).

%% The same gen_server implementation supports three different kinds
%% of processes: an actual transport process, one that will club it to
%% death should the parent die before a connection is established, and
%% a process owning the listening port.

%% Listener process state.
-record(listener, {socket    :: inet:socket(),
                   count = 1 :: non_neg_integer(),
                   tref      :: reference()}).

%% Monitor process state.
-record(monitor,
        {parent             :: pid(),
         transport = self() :: pid()}).

-type tref()   :: reference().  %% timer reference
-type length() :: 0..16#FFFFFF. %% message length from Diameter header
-type size()   :: non_neg_integer().  %% accumulated binary size
-type frag()   :: {length(), size(), binary(), list(binary())}
                | binary().

%% Accepting/connecting transport process state.
-record(transport,
        {socket  :: inet:socket(),  %% accept or connect socket
         parent  :: pid(),          %% of process that started us
         module  :: module(),       %% gen_tcp-like module
         frag = <<>> :: binary() | {tref(), frag()}}). %% message fragment

%% The usual transport using gen_tcp can be replaced by anything
%% sufficiently gen_tcp-like by passing a 'module' option as the first
%% (for simplicity) transport option. The transport_module diameter_etcp
%% uses this to set itself as the module to call, its start/3 just
%% calling start/3 here with the option set.

%% ---------------------------------------------------------------------------
%% # start/3
%% ---------------------------------------------------------------------------

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
%% # init/1
%% ---------------------------------------------------------------------------

init(T) ->
    gen_server:enter_loop(?MODULE, [], i(T)).

%% i/1

%% A transport process.
i({T, Ref, Mod, Pid, Opts, Addrs})
  when T == accept;
       T == connect ->
    erlang:monitor(process, Pid),
    %% Since accept/connect might block indefinitely, spawn a process
    %% that does nothing but kill us with the parent until call
    %% returns.
    {ok, MPid} = diameter_tcp_sup:start_child(#monitor{parent = Pid}),
    Sock = i(T, Ref, Mod, Pid, Opts, Addrs),
    MPid ! {stop, self()},  %% tell the monitor to die
    setopts(Mod, Sock),
    #transport{parent = Pid,
               module = Mod,
               socket = Sock};

%% A monitor process to kill the transport if the parent dies.
i(#monitor{parent = Pid, transport = TPid} = S) ->
    proc_lib:init_ack({ok, self()}),
    erlang:monitor(process, Pid),
    erlang:monitor(process, TPid),
    S;
%% In principle a link between the transport and killer processes
%% could do the same thing: have the accepting/connecting process be
%% killed when the killer process dies as a consequence of parent
%% death. However, a link can be unlinked and this is exactly what
%% gen_tcp seems to so. Links should be left to supervisors.

i({listen, LRef, APid, {Mod, Opts, Addrs}}) ->
    {[LA, LP], Rest} = proplists:split(Opts, [ip, port]),
    LAddr = get_addr(LA, Addrs),
    LPort = get_port(LP),
    {ok, LSock} = Mod:listen(LPort, gen_opts(LAddr, Rest)),
    proc_lib:init_ack({ok, self(), {LAddr, LSock}}),
    erlang:monitor(process, APid),
    true = diameter_reg:add_new({?MODULE, listener, {LRef, {LAddr, LSock}}}),
    start_timer(#listener{socket = LSock}).

%% i/6

i(accept, Ref, Mod, Pid, Opts, Addrs) ->
    {LAddr, LSock} = listener(Ref, {Mod, Opts, Addrs}),
    proc_lib:init_ack({ok, self(), [LAddr]}),
    Sock = ok(accept(Mod, LSock)),
    diameter_peer:up(Pid),
    Sock;

i(connect, _, Mod, Pid, Opts, Addrs) ->
    {[LA, RA, RP], Rest} = proplists:split(Opts, [ip, raddr, rport]),
    LAddr = get_addr(LA, Addrs),
    RAddr = get_addr(RA, []),
    RPort = get_port(RP),
    proc_lib:init_ack({ok, self(), [LAddr]}),
    Sock = ok(connect(Mod, RAddr, RPort, gen_opts(LAddr, Rest))),
    diameter_peer:up(Pid, {RAddr, RPort}),
    Sock.

ok({ok, T}) ->
    T;
ok(No) ->
    x(No).

x(Reason) ->
    exit({shutdown, Reason}).

%% listener/2

listener(LRef, T) ->
    l(diameter_reg:match({?MODULE, listener, {LRef, '_'}}), LRef, T).

%% Existing process with the listening socket ...
l([{{?MODULE, listener, {_, AS}}, LPid}], _, _) ->
    LPid ! {accept, self()},
    AS;

%% ... or not: start one.
l([], LRef, T) ->
    {ok, _, AS} = diameter_tcp_sup:start_child({listen, LRef, self(), T}),
    AS.

%% get_addr/2

get_addr(As, Def) ->
    diameter_lib:ipaddr(addr(As, Def)).

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

gen_opts(LAddr, Opts) ->
    {L,_} = proplists:split(Opts, [binary, packet, active]),
    [[],[],[]] == L orelse ?ERROR({reserved_options, Opts}),
    [binary,
     {packet, 0},
     {active, once},
     {ip, LAddr}
     | Opts].

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

%% start_timer/1

start_timer(#listener{count = 0} = S) ->
    S#listener{tref = erlang:start_timer(?LISTENER_TIMEOUT, self(), close)};
start_timer(S) ->
    S.

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

%% Another accept transport is attaching.
l({accept, TPid}, #listener{count = N} = S) ->
    erlang:monitor(process, TPid),
    S#listener{count = N+1};

%% Accepting process has died.
l({'DOWN', _, process, _, _}, #listener{count = N} = S) ->
    start_timer(S#listener{count = N-1});

%% Timeout after the last accepting process has died.
l({timeout, TRef, close = T}, #listener{tref = TRef,
                                        count = 0}) ->
    x(T);
l({timeout, _, close}, #listener{} = S) ->
    S.

%% t/2
%%
%% Transition transport state.

t(T,S) ->
    case transition(T,S) of
        ok ->
            S;
        #transport{} = NS ->
            NS;
        {stop, Reason} ->
            x(Reason);
        stop ->
            x(T)
    end.

%% transition/2

%% Incoming message.
transition({tcp, Sock, Data}, #transport{socket = Sock,
                                         module = M}
                              = S) ->
    setopts(M, Sock),
    recv(Data, S);

transition({tcp_closed, Sock}, #transport{socket = Sock}) ->
    stop;

transition({tcp_error, Sock, _Reason} = T, #transport{socket = Sock} = S) ->
    ?ERROR({T,S});

%% Outgoing message.
transition({diameter, {send, Bin}}, #transport{socket = Sock,
                                               module = M}) ->
    case send(M, Sock, Bin) of
        ok ->
            ok;
        {error, Reason} ->
            {stop, {send, Reason}}
    end;

%% Request to close the transport connection.
transition({diameter, {close, Pid}}, #transport{parent = Pid,
                                                socket = Sock,
                                                module = M}) ->
    M:close(Sock),
    stop;

%% Timeout for reception of outstanding packets.
transition({timeout, TRef, flush}, S) ->
    flush(TRef, S);

%% Request for the local port number.
transition({resolve_port, RPid}, #transport{socket = Sock,
                                            module = M})
  when is_pid(RPid) ->
    RPid ! lport(M, Sock),
    ok;

%% Parent process has died.
transition({'DOWN', _, process, Pid, _}, #transport{parent = Pid}) ->
    stop.

%% Crash on anything unexpected.

%% recv/2
%%
%% Reassemble fragmented messages and extract multple message sent
%% using Nagle.

recv(Bin, #transport{parent = Pid, frag = Head} = S) ->
    S#transport{frag = recv(Pid, Head, Bin)}.

%% recv/3

%% No previous fragment.
recv(Pid, <<>>, Bin) ->
    rcv(Pid, Bin);

recv(Pid, {TRef, Head}, Bin) ->
    erlang:cancel_timer(TRef),
    rcv(Pid, Head, Bin).

%% rcv/3

%% Not even the first four bytes of the header.
rcv(Pid, Head, Bin)
  when is_binary(Head) ->
    rcv(Pid, <<Head/binary, Bin/binary>>);

%% Or enough to know how many bytes to extract.
rcv(Pid, {Len, N, Head, Acc}, Bin) ->
    rcv(Pid, Len, N + size(Bin), Head, [Bin | Acc]).

%% rcv/5

%% Extract a message for which we have all bytes.
rcv(Pid, Len, N, Head, Acc)
  when Len =< N ->
    rcv(Pid, rcv1(Pid, Len, bin(Head, Acc)));

%% Wait for more packets.
rcv(_, Len, N, Head, Acc) ->
    {start_timer(), {Len, N, Head, Acc}}.

%% rcv/2

%% Nothing left.
rcv(_, <<>> = Bin) ->
    Bin;

%% Well, this isn't good. Chances are things will go south from here
%% but if we're lucky then the bytes we have extend to an intended
%% message boundary and we can recover by simply discarding them,
%% which is the result of receiving them.
rcv(Pid, <<_:1/binary, Len:24, _/binary>> = Bin)
  when Len < 20 ->
    diameter_peer:recv(Pid, Bin),
    <<>>;

%% Enough bytes to extract a message.
rcv(Pid, <<_:1/binary, Len:24, _/binary>> = Bin)
  when Len =< size(Bin) ->
    rcv(Pid, rcv1(Pid, Len, Bin));

%% Or not: wait for more packets.
rcv(_, <<_:1/binary, Len:24, _/binary>> = Head) ->
    {start_timer(), {Len, size(Head), Head, []}};

%% Not even 4 bytes yet.
rcv(_, Head) ->
    {start_timer(), Head}.

%% rcv1/3

rcv1(Pid, Len, Bin) ->
    <<Msg:Len/binary, Rest/binary>> = Bin,
    diameter_peer:recv(Pid, Msg),
    Rest.

%% bin/[12]

bin(Head, Acc) ->
    list_to_binary([Head | lists:reverse(Acc)]).

bin({_, _, Head, Acc}) ->
    bin(Head, Acc);
bin(Bin)
  when is_binary(Bin) ->
    Bin.

%% start_timer/0

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

start_timer() ->
    erlang:start_timer(?FRAGMENT_TIMEOUT, self(), flush).

flush(TRef, #transport{parent = Pid, frag = {TRef, Head}} = S) ->
    diameter_peer:recv(Pid, bin(Head)),
    S#transport{frag = <<>>};
flush(_, S) ->
    S.

%% accept/2

accept(gen_tcp, LSock) ->
    gen_tcp:accept(LSock);
accept(Mod, LSock) ->
    Mod:accept(LSock).

%% connect/4

connect(gen_tcp, Host, Port, Opts) ->
    gen_tcp:connect(Host, Port, Opts);
connect(Mod, Host, Port, Opts) ->
    Mod:connect(Host, Port, Opts).

%% send/3

send(gen_tcp, Sock, Bin) ->
    gen_tcp:send(Sock, Bin);
send(M, Sock, Bin) ->
    M:send(Sock, Bin).

%% setopts/3

setopts(gen_tcp, Sock, Opts) ->
    inet:setopts(Sock, Opts);
setopts(M, Sock, Opts) ->
    M:setopts(Sock, Opts).

%% setopts/2

setopts(M, Sock) ->
    case setopts(M, Sock, [{active, once}]) of
        ok -> ok;
        X  -> x({setopts, M, Sock, X})  %% possibly on peer disconnect
    end.

%% lport/2

lport(gen_tcp, Sock) ->
    inet:port(Sock);
lport(M, Sock) ->
    M:port(Sock).
