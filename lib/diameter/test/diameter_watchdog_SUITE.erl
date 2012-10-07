%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2012. All Rights Reserved.
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
%% Tests of the RFC3539 watchdog state machine as implemented by
%% module diameter_watchdog.
%%

-module(diameter_watchdog_SUITE).

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([reopen/1, reopen/4, reopen/7]).

-export([id/1,    %% jitter callback
         run1/1]).

%% diameter_app callbacks
-export([peer_up/3,
         peer_down/3]).

%% gen_tcp-ish interface
-export([listen/2,
         accept/1,
         connect/3,
         send/2,
         setopts/2]).

-include("diameter.hrl").
-include("diameter_ct.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(BASE, ?DIAMETER_DICT_COMMON).
-define(REALM, "erlang.org").
-define(ADDR, {127,0,0,1}).

%% Config for diameter:start_service/2.
-define(SERVICE(Name),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 42},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {application, [{alias, Name},
                        {dictionary, ?BASE},
                        {module, ?MODULE}]}]).

%% Watchdog timer as a callback.
-define(WD(T), {?MODULE, id, [T]}).

%% Watchdog timers used by the testcases. Note that the short timeout
%% with random jitter is excluded since the reopen/1 isn't smart
%% enough to deal with it: see ONE_WD below.
-define(WD_TIMERS, [?WD(6000)
                    | [F_(T_) || T_ <- [10000, 20000, 30000],
                                 F_ <- [fun(T__) -> T__ end,
                                        fun(T__) -> ?WD(T__) end]]]).

%% Watchdog timer of the misbehaving peer.
-define(PEER_WD, 10000).

%% Receive a watchdog event within a specified time.
-define(EVENT(T, Tmo),
        receive #diameter_event{info = T} -> now()
        after Tmo -> ?ERROR({timeout, Tmo})
        end).

%% Receive an event in a given number of watchdogs, plus or minus
%% half. Note that the call to now_diff assumes left to right
%% evaluation order.
-define(EVENT(T, N, WdL, WdH),
        [?ERROR({received, _Elapsed_, _LowerBound_, N, WdL})
         || _UpperBound_ <- [(N)*(WdH) + (WdH) div 2],
            _Elapsed_    <- [now_diff(now(), ?EVENT(T, _UpperBound_))],
            _LowerBound_ <- [(N)*(WdL) - (WdL) div 2],
            _Elapsed_ =< _LowerBound_*1000]).

-define(EVENT(T, N, Wd),
        ?EVENT(T, N, Wd, Wd)).

%% A timeout that ensures one watchdog. The ensure only one watchdog
%% requires (Wd + 2000) + 1000 < 2*(Wd - 2000) ==> 7000 < Wd for the
%% case with random jitter.
-define(ONE_WD(Wd), jitter(Wd,2000) + 1000).

%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 10}}].%% enough for 17 watchdogs @ 30 sec plus jitter

all() ->
    [reopen].

init_per_suite(Config) ->
    ok = diameter:start(),
    Config.

end_per_suite(_Config) ->
    ok = diameter:stop().

%% ===========================================================================
%% # reopen/1
%% ===========================================================================

%% Test the watchdog state machine for the required failover, failback
%% and reopen behaviour. Do this by having the testcase replace
%% diameter_service and start watchdogs, and having this module
%% implement a transport process that plays the role of the peer
%% Diameter node.

%reopen(_) ->
%    reopen(connect, ?WD(10000), 1, 'DWR');

reopen(_) ->
    [] = run([[reopen, T, Wd, N, M]
              || Wd <- ?WD_TIMERS,       %% watchdog_timer value
                 T <- [listen, connect], %% watchdog to test
                 N <- [0,1,2],           %% DWR's to answer before ignoring
                 M <- ['DWR', 'DWA', 'RAA']]). %% how to induce failback

reopen(Type, Wd, N, M) ->
    Server = start_service(),
    Client = start_service(),

    %% The peer to the transport whose watchdog is tested is given a
    %% long watchdog timeout so that it doesn't send DWR of its own.
    {Node, Peer} = {{[], Wd}, {[{module, ?MODULE}], ?WD(?PEER_WD)}},

    {{LH,LW},{CH,CW}} = case Type of
                            listen  -> {Node, Peer};
                            connect -> {Peer, Node}
                        end,

    LO = [{transport_module, diameter_tcp},
          {transport_config, LH ++ [{ip, ?ADDR}, {port, 0}]},
          {watchdog_timer, LW}],

    {ok, LRef} = diameter:add_transport(Server, {listen, LO}),

    [LP] = ?util:lport(tcp, LRef, 20),

    CO = [{transport_module, diameter_tcp},
          {transport_config, CH ++ [{ip, ?ADDR}, {port, 0},
                                    {raddr, ?ADDR}, {rport, LP}]},
          {watchdog_timer, CW}],

    %% Use a temporary process to ensure the connecting transport is
    %% added only once events from the listening transport are
    %% subscribed to.
    Pid = spawn(fun() -> receive _ -> ok end end),

    [] = run([[reopen, Type, T, LRef, Pid, Wd, N, M]
              || T <- [{listen, Server}, {connect, Client, CO}]]).

%% start_service/1

start_service() ->
    Name = hostname(),
    ok = diameter:start_service(Name, [{monitor, self()} | ?SERVICE(Name)]),
    Name.

%% reopen/7

reopen(Type, {listen = T, SvcName}, Ref, Pid, Wd, N, M) ->
    diameter:subscribe(SvcName),
    Pid ! ok,
    recv(Type, T, SvcName, Ref, Wd, N, M);

reopen(Type, {connect = T, SvcName, Opts}, _, Pid, Wd, N, M) ->
    diameter:subscribe(SvcName),
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, process, _, _} -> ok end,
    {ok, Ref} = diameter:add_transport(SvcName, {T, Opts}),
    recv(Type, T, SvcName, Ref, Wd, N, M).

%% recv/7

%% The watchdog to be tested.
recv(Type, Type, _SvcName, Ref, Wd, N, M) ->
    %% Connection should come up immediately as a consequence of
    %% starting the watchdog process. In the accepting case this
    %% results in a new watchdog on a transport waiting for a new
    %% connection.

    ?EVENT({watchdog, Ref, _, {initial, okay}, _}, 2000),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}, 0),

    %% Low/high watchdog timeouts.
    WdL = jitter(Wd, -2000),
    WdH = jitter(Wd, 2000),

    %%   OKAY          Timer expires &      Failover()
    %%                 Pending              SetWatchdog()        SUSPECT
    %%
    %% The peer replies to N DWR's before becoming silent, we should
    %% go down after N+2 watchdog_timer expirations: that is, after
    %% the first unanswered DWR. Knowing the min/max watchdog timeout
    %% values gives the time interval in which the event is expected.

    ?EVENT({watchdog, Ref, _, {okay, suspect}, _}, N+2, WdL, WdH),
    ?EVENT({down, Ref, _, _}, 0),

    %%   SUSPECT       Receive DWA          Pending = FALSE
    %%                                      Failback()
    %%                                      SetWatchdog()        OKAY
    %%
    %%   SUSPECT       Receive non-DWA      Failback()
    %%                                      SetWatchdog()        OKAY
    %%
    %% The peer sends a message before the expiry of another watchdog
    %% to induce failback.

    ?EVENT({watchdog, Ref, _, {suspect, okay}, _}, WdH + 2000),
    ?EVENT({up, Ref, _, _}, 0),

    %%   OKAY          Timer expires &      SendWatchdog()
    %%                 !Pending             SetWatchdog()
    %%                                      Pending = TRUE       OKAY
    %%
    %%   OKAY          Timer expires &      Failover()
    %%                 Pending              SetWatchdog()        SUSPECT
    %%
    %% The peer is now ignoring all watchdogs so the connection goes
    %% back down after either one or two watchdog expiries, depending
    %% on whether or not DWA restored the connection.

    F = choose(M == 'DWA', 2, 1),
    ?EVENT({watchdog, Ref, _, {okay, suspect}, _}, F, WdL, WdH),
    ?EVENT({down, Ref, _, _}, 0),

    %%   SUSPECT       Timer expires        CloseConnection()
    %%                                      SetWatchdog()        DOWN
    %%
    %% Non-response brings the connection down after another timeout.

    ?EVENT({watchdog, Ref, _, {suspect, down}, _}, 1, WdL, WdH),

    %%   DOWN          Timer expires        AttemptOpen()
    %%                                      SetWatchdog()        DOWN
    %%
    %%   DOWN          Connection up        NumDWA = 0
    %%                                      SendWatchdog()
    %%                                      SetWatchdog()
    %%                                      Pending = TRUE       REOPEN
    %%
    %% The connection is reestablished after another timeout.

    recv_reopen(Type, Ref, WdL, WdH),

    %%   REOPEN        Receive non-DWA      Throwaway()          REOPEN
    %%
    %%   REOPEN        Receive DWA &        Pending = FALSE
    %%                 NumDWA < 2           NumDWA++             REOPEN
    %%
    %%   REOPEN        Receive DWA &        Pending = FALSE
    %%                 NumDWA == 2          NumDWA++
    %%                                      Failback()           OKAY
    %%
    %%   REOPEN        Timer expires &      SendWatchdog()
    %%                 !Pending             SetWatchdog()
    %%                                      Pending = TRUE       REOPEN
    %%
    %% An exchange of 3 watchdogs (the first directly after
    %% capabilities exchange) brings the connection back up.

    ?EVENT({watchdog, Ref, _, {reopen, okay}, _}, 2, WdL, WdH),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}, 0),

    %% Non-response brings it down again.

    ?EVENT({watchdog, Ref, _, {okay, suspect}, _}, 2, WdL, WdH),
    ?EVENT({down, Ref, _, _}, 0),
    ?EVENT({watchdog, Ref, _, {suspect, down}, _}, 1, WdL, WdH),

    %% Reestablish after another watchdog.

    recv_reopen(Type, Ref, WdL, WdH),

    %%   REOPEN        Timer expires &      NumDWA = -1
    %%                 Pending &            SetWatchdog()
    %%                 NumDWA >= 0                               REOPEN
    %%
    %%   REOPEN        Timer expires &      CloseConnection()
    %%                 Pending &            SetWatchdog()
    %%                 NumDWA < 0                                DOWN
    %%
    %% Peer is now ignoring all watchdogs go down again after 2
    %% timeouts.

    ?EVENT({watchdog, Ref, _, {reopen, down}, _}, 2, WdL, WdH);

%% The misbehaving peer.
recv(_, Type, SvcName, Ref, Wd, N, M) ->
    %% First transport process.
    ?EVENT({watchdog, Ref, _, {initial, okay}, _}, 1000),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}, 0),
    reg(Type, Ref, SvcName, {SvcName, {Wd,N,M}}),
    ?EVENT({watchdog, Ref, _, {okay, down}, _}, infinity),

    %% Second transport process.
    ?EVENT({watchdog, Ref, _, {_, reopen}, _}, infinity),
    reg(Type, Ref, SvcName, 3),
    ?EVENT({watchdog, Ref, _, {_, down}, _}, infinity),

    %% Third transport process.
    ?EVENT({watchdog, Ref, _, {_, reopen}, _}, infinity),
    reg(Type, Ref, SvcName, 0),
    ?EVENT({watchdog, Ref, _, {_, down}, _}, infinity),

    ok.

%% recv_reopen/4

recv_reopen(connect, Ref, WdL, WdH) ->
    ?EVENT({watchdog, Ref, _, {_, reopen}, _}, 1, WdL, WdH),
    ?EVENT({reconnect, Ref, _}, 0);

recv_reopen(listen, Ref, _, _) ->
    ?EVENT({watchdog, Ref, _, {_, reopen}, _}, 1, ?PEER_WD).

%% reg/4
%%
%% Lookup the pid of the transport process and publish a term for
%% send/2 to lookup.
reg(Type, Ref, SvcName, T) ->
    TPid = tpid(Type, Ref, diameter:service_info(SvcName, transport)),
    true = diameter_reg:add_new({?MODULE, TPid, T}).
    
%% tpid/3

tpid(connect, Ref, [[{ref, Ref},
                     {type, connect},
                     {options, _},
                     {watchdog, _},
                     {peer, _},
                     {apps, _},
                     {caps, _},
                     {port, [{owner, TPid} | _]}
                     | _]]) ->
    TPid;

tpid(listen, Ref, [[{ref, Ref},
                    {type, listen},
                    {options, _},
                    {accept, As}
                    | _]]) ->
    [[{watchdog, _},
      {peer, _},
      {apps, _},
      {caps, _},
      {port, [{owner, TPid} | _]}
      | _]]
        = lists:filter(fun([{watchdog, {_,_,S}} | _]) ->
                               S == okay orelse S == reopen 
                       end,
                       As),
    TPid.

%% ===========================================================================

listen(PortNr, Opts) ->
    gen_tcp:listen(PortNr, Opts).

accept(LSock) ->
    gen_tcp:accept(LSock).

connect(Addr, Port, Opts) ->
    gen_tcp:connect(Addr, Port, Opts).

setopts(Sock, Opts) ->
    inet:setopts(Sock, Opts).

send(Sock, Bin) ->
    send(getr(config), Sock, Bin).

%% send/3

%% First outgoing message from a new transport process is CER/CEA.
%% Remaining outgoing messages are either DWR or DWA.
send(undefined, Sock, Bin) ->
    putr(config, init),
    gen_tcp:send(Sock, Bin);

%% Outgoing DWR: fake reception of DWA. Use the fact that AVP values
%% are ignored. This is to ensure that the peer's watchdog state
%% transitions are only induced by responses to messages it sends.
send(_, Sock, <<_:32, 1:1, _:7, 280:24, _:32, EId:32, HId:32, _/binary>>) ->
    Pkt = #diameter_packet{header = #diameter_header{version = 1,
                                                     end_to_end_id = EId,
                                                     hop_by_hop_id = HId},
                           msg = ['DWA', {'Result-Code', 2001},
                                         {'Origin-Host', "XXX"},
                                         {'Origin-Realm', ?REALM}]},
    #diameter_packet{bin = Bin} = diameter_codec:encode(?BASE, Pkt),
    self() ! {tcp, Sock, Bin},
    ok;

%% First outgoing DWA.
send(init, Sock, Bin) ->
    [{{?MODULE, _, T}, _}] = diameter_reg:wait({?MODULE, self(), '_'}),
    putr(config, T),
    send(Sock, Bin);

%% First transport process.
send({SvcName, {_,_,_} = T}, Sock, Bin) ->
    [{'Origin-Host', _} = OH, {'Origin-Realm', _} = OR | _]
        = ?SERVICE(SvcName),
    putr(origin, [OH, OR]),
    putr(config, T),
    send(Sock, Bin);
    
%% Discard DWA, failback after another timeout in the peer.
send({Wd, 0 = No, Msg}, Sock, Bin) ->
    Origin = getr(origin),
    spawn(fun() -> failback(?ONE_WD(Wd), Msg, Sock, Bin, Origin) end),
    putr(config, No),
    ok;

%% Send DWA while we're in the mood (aka 0 < N).
send({Wd, N, Msg}, Sock, Bin) ->
    putr(config, {Wd, N-1, Msg}),
    gen_tcp:send(Sock, Bin);

%% Discard DWA.
send(0, _Sock, _Bin) ->
    ok;

%% Send DWA.
send(N, Sock, <<_:32, 0:1, _:7, 280:24, _/binary>> = Bin) ->
    putr(config, N-1),
    gen_tcp:send(Sock, Bin).

failback(Tmo, Msg, Sock, Bin, Origin) ->
    timer:sleep(Tmo),
    ok = gen_tcp:send(Sock, msg(Msg, Bin, Origin)).

%% msg/2

msg('DWA', Bin, _Origin) ->
    Bin;
msg(Msg, _Bin, Origin) ->
    #diameter_packet{bin = Bin}
        = diameter_codec:encode(?BASE, msg(Msg, Origin)),
    Bin.

msg('DWR' = M, T) ->
    [M | T];

msg('RAA', T) ->
    ['RAA', {'Session-Id', diameter:session_id("abc")},
            {'Result-Code', 2001}
          | T].
%% An unexpected answer is discarded after passing through the
%% watchdog state machine.

%% ===========================================================================

peer_up(_SvcName, _Peer, S) ->
    S.

peer_down(_SvcName, _Peer, S) ->
    S.

%% ===========================================================================

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

%% id/1
%%
%% Jitter callback.

id(T) ->
    T.

%% run/1
%%
%% A more useful badmatch in case of failure.

run(Fs) ->
    ?util:run([{?MODULE, [run1, F]} || F <- Fs]).

run1([F|A]) ->
    ok = try
             apply(?MODULE, F, A),
             ok
         catch
             E:R ->
                 S = erlang:get_stacktrace(),
                 io:format("~p~n", [{A, E, R, S}]),
                 S
         end.

%% now_diff/2

now_diff(T1, T2) ->
    timer:now_diff(T2, T1).

%% jitter/2

jitter(?WD(T), _) ->
    T;
jitter(T,D) ->
    T+D.

%% Generate a unique hostname for the faked peer.
hostname() ->
    lists:flatten(io_lib:format("~p-~p-~p", tuple_to_list(now()))).

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).
