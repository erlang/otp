%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
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
-export([reopen/1, reopen/4, reopen/6]).

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
         {'Auth-Application-Id', [0 = ?BASE:id()]},
         {application, [{alias, Name},
                        {dictionary, ?BASE},
                        {module, ?MODULE}]}]).

%% Watchdog timer as a callback.
-define(WD(T), {?MODULE, id, [T]}).

%% Watchdog timers used by the testcases.
-define(WD_TIMERS, [10000, ?WD(10000)]).

%% Watchdog timer of the misbehaving node. Longer than the peer's so
%% that DWR is never sent at watchdog timeout.
-define(PEER_WD, 20000).

%% A timeout that ensures one watchdog. To ensure only one watchdog
%% requires (Wd + 2000) + 1000 < 2*(Wd - 2000) ==> 7000 < Wd for the
%% case with random jitter.
-define(ONE_WD(Wd), jitter(Wd,2000) + 1000).

%% Receive an event message from diameter.
-define(EVENT(T),
        apply(fun() ->  %% apply to not bind T_
                      receive #diameter_event{info = T = T_} ->
                              log_event(T_)
                      end
              end,
              [])).

%% Receive a watchdog event.
-define(WD_EVENT(Ref), log_wd(element(4, ?EVENT({watchdog, Ref, _, _, _})))).

%% Log to make failures identifiable.
-define(LOG(T),   ?LOG("~p", [T])).
-define(LOG(F,A), ct:pal("~p: " ++ F, [self() | A])).

%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 5}}]. %% 20 watchdogs @ 15 sec

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
%% and reopen behaviour by examining watchdog events.

reopen(_) ->
    [] = run([[reopen, T, W, N, M]
              || T <- [listen, connect], %% watchdog to test
                 W <- ?WD_TIMERS,        %% watchdog_timer value
                 N <- [0,1,2],           %% DWR's to answer before ignoring
                 M <- ['DWR', 'DWA', 'RAA']]). %% how to induce failback

reopen(Test, Wd, N, M) ->
    %% Publish a ref ensure the connecting transport is added only
    %% once events from the listening transport are subscribed to.
    Ref = make_ref(),
    [] = run([[reopen, T, Test, Ref, Wd, N, M] || T <- [listen, connect]]).

%% reopen/6

reopen(Type, Test, Ref, Wd, N, M) ->
    {SvcName, TRef} = start(Type, Ref, opts(Type, Test, Ref, Wd)),
    reopen(Type, Test, SvcName, TRef, Wd, N, M).

%% start/3

start(Type, Ref, Opts) ->
    Name = hostname(),
    true = diameter:subscribe(Name),
    ok = diameter:start_service(Name, [{monitor, self()} | ?SERVICE(Name)]),
    {ok, TRef} = diameter:add_transport(Name, {Type, Opts}),
    true = diameter_reg:add_new({Type, Ref, Name}),
    {Name, TRef}.

opts(Type, Test, Ref, Wd) ->
    {Timer, Counts, Mod} = opts(Type, Test, Wd),
    [{transport_module, diameter_tcp},
     {transport_config, Mod ++ [{ip, ?ADDR}, {port, 0}] ++ cfg(Type, Ref)},
     {watchdog_timer, Timer},
     {watchdog_counts, Counts}].

cfg(listen, _) ->
    [];
cfg(connect, Ref) ->
    [{{_, _, SvcName}, _Pid}] = diameter_reg:wait({listen, Ref, '_'}),
    [[{ref, LRef} | _]] = diameter:service_info(SvcName, transport),
    [LP] = ?util:lport(tcp, LRef, 20),
    [{raddr, ?ADDR}, {rport, LP}].

opts(Type, Type, Wd) ->
    {Wd, [], []};
opts(_Type, _Test, _Wd) ->
    {?WD(?PEER_WD), [{open, 0}], [{module, ?MODULE}]}.

%% reopen/7

%% The watchdog to be tested.
reopen(Type, Type, SvcName, Ref, Wd, N, M) ->
    ?LOG("node, ~p, ~p", [Type, Ref]),

    %% Connection should come up immediately as a consequence of
    %% starting the watchdog process. In the accepting case this
    %% results in a new watchdog on a transport waiting for a new
    %% connection.

    {initial, okay} = ?WD_EVENT(Ref),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}),

    %%   OKAY          Timer expires &      Failover()
    %%                 Pending              SetWatchdog()        SUSPECT
    %%
    %% The peer replies to N DWR's before becoming silent, we should
    %% go down after N+2 watchdog_timer expirations: that is, after
    %% the first unanswered DWR. Knowing the min/max watchdog timeout
    %% values gives the time interval in which the event is expected.

    [0,0,0,0] = wd_counts(SvcName),

    {okay, suspect} = ?WD_EVENT(Ref),
    ?EVENT({down, Ref, _, _}),

    %% N received DWA's
    [_,_,_,N] = wd_counts(SvcName),

    %%   SUSPECT       Receive DWA          Pending = FALSE
    %%                                      Failback()
    %%                                      SetWatchdog()        OKAY
    %%
    %%   SUSPECT       Receive non-DWA      Failback()
    %%                                      SetWatchdog()        OKAY
    %%
    %% The peer sends a message before the expiry of another watchdog
    %% to induce failback.

    {suspect, okay} = ?WD_EVENT(Ref),
    ?EVENT({up, Ref, _, _}),

    %% N+1 sent DWR's, N/N+1 received DWA's
    R1 = N+1,
    A1 = choose(M == 'DWA', R1, N),
    [R1,_,_,A1] = wd_counts(SvcName),

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

    {okay, suspect} = ?WD_EVENT(Ref),
    ?EVENT({down, Ref, _, _}),

    %%   SUSPECT       Timer expires        CloseConnection()
    %%                                      SetWatchdog()        DOWN
    %%
    %% Non-response brings the connection down after another timeout.

    {suspect, down} = ?WD_EVENT(Ref),

    R2 = R1 + choose(M == 'DWA', 1, 0),
    A2 = A1,
    [R2,_,_,A2] = wd_counts(SvcName),

    %%   DOWN          Timer expires        AttemptOpen()
    %%                                      SetWatchdog()        DOWN
    %%
    %%   DOWN          Connection up        NumDWA = 0
    %%                                      SendWatchdog()
    %%                                      SetWatchdog()
    %%                                      Pending = TRUE       REOPEN
    %%
    %% The connection is reestablished after another timeout.

    recv_reopen(Type, Ref),

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

    {reopen, okay} = ?WD_EVENT(Ref),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}),

    %% Three DWR's have been answered.
    R3 = R2 + 3,
    A3 = A2 + 3,
    [R3,_,_,A3] = wd_counts(SvcName),

    %% Non-response brings it down again.

    {okay, suspect} = ?WD_EVENT(Ref),
    ?EVENT({down, Ref, _, _}),
    {suspect, down} = ?WD_EVENT(Ref),

    R4 = R3 + 1,
    A4 = A3,
    [R4,_,_,A4] = wd_counts(SvcName),

    %% Reestablish after another watchdog.

    recv_reopen(Type, Ref),

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

    {reopen, down} = ?WD_EVENT(Ref);

%% The misbehaving peer.
reopen(Type, _, SvcName, Ref, Wd, N, M) ->
    ?LOG("peer, ~p, ~p", [Type, Ref]),

    %% First transport process.
    {initial, okay} = ?WD_EVENT(Ref),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}),

    reg(Type, Ref, SvcName, {SvcName, {Wd,N,M}}),

    {okay, down} = ?WD_EVENT(Ref),

    %% Second transport process.
    ?EVENT({watchdog, Ref, _, {_, reopen}, _}),
    reg(Type, Ref, SvcName, 3),
    ?EVENT({watchdog, Ref, _, {_, down}, _}),

    %% Third transport process.
    ?EVENT({watchdog, Ref, _, {_, reopen}, _}),
    reg(Type, Ref, SvcName, 0),
    ?EVENT({watchdog, Ref, _, {_, down}, _}),

    ok.

log_wd({From, To} = T) ->
    ?LOG("~p -> ~p", [From, To]),
    T.

log_event(E) ->
    T = element(1,E),
    T == watchdog orelse ?LOG("~p", [T]),
    E.

%% wd_counts/1

wd_counts(SvcName) ->
    [Info] = diameter:service_info(SvcName, transport),
    {_, Counters} = lists:keyfind(statistics, 1, Info),
    [proplists:get_value({{0,280,R}, D}, Counters, 0) || D <- [send,recv],
                                                         R <- [1,0]].

%% recv_reopen/2

recv_reopen(connect, Ref) ->
    {down, reopen} = ?WD_EVENT(Ref),
    ?EVENT({reconnect, Ref, _});

recv_reopen(listen, Ref) ->
    {_, reopen} = ?WD_EVENT(Ref).

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
