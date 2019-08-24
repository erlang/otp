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
-export([reopen/0, reopen/1, reopen/4, reopen/6,
         suspect/1, suspect/4,
         okay/1, okay/4]).

-export([id/1,    %% jitter callback
         run1/1,
         abuse/1,
         abuse/2]).

%% diameter_app callbacks
-export([peer_up/3,
         peer_down/3]).

%% diameter_tcp message_cb
-export([message/3]).

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

%% Watchdog timer of the misbehaving node.
-define(PEER_WD, 10000).

%% A timeout that ensures one watchdog. To ensure only one watchdog
%% requires (Wd + 2000) + 1000 < 2*(Wd - 2000) ==> 7000 < Wd for the
%% case with random jitter.
-define(ONE_WD(Wd), jitter(Wd,2000) + 1000).
-define(INFO(T), #diameter_event{info = T}).

%% Receive an event message from diameter.
-define(EVENT(T),    %% apply to not bind T_
        apply(fun() ->
                      receive ?INFO(T = T_) -> log_event(T_) end
              end,
              [])).

%% Receive a watchdog event.
-define(WD_EVENT(Ref), log_wd(element(4, ?EVENT({watchdog, Ref, _, _, _})))).
-define(WD_EVENT(Ref, Ms),
        apply(fun() ->
                      receive ?INFO({watchdog, Ref, _, T_, _}) ->
                              log_wd(T_)
                      after Ms ->
                              false
                      end
              end,
              [])).

%% Log to make failures identifiable.
-define(LOG(T),     ?LOG("~p", [T])).
-define(LOG(F,A),   ct:pal("~p: " ++ F, [self() | A])).
-define(WARN(F,A),  ct:pal(error, "~p: " ++ F, [self() | A])).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 90}}].

all() ->
    [reopen,
     suspect,
     okay].

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

reopen() ->
    [{timetrap, {minutes, 5}}]. %% 20 watchdogs @ 15 sec

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
    {SvcName, TRef} = start(Type, Ref, cfg(Type, Test, Wd)),
    reopen(Type, Test, SvcName, TRef, Wd, N, M).

cfg(Type, Type, Wd) ->
    {Wd, [], false};
cfg(_Type, _Test, _Wd) ->
    {?WD(?PEER_WD), [{okay, 0}], true}.

%% reopen/7

%% The watchdog to be tested.
reopen(Type, Type, SvcName, Ref, Wd, N, M) ->
    ?LOG("node ~p", [[Type, SvcName, Ref, Wd, N, M]]),

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
    ?LOG("peer ~p", [[Type, SvcName, Ref, Wd, N, M]]),

    %% First transport process.
    {initial, okay} = ?WD_EVENT(Ref),
    ?EVENT({up, Ref, _, _, #diameter_packet{}}),

    reg(Ref, SvcName, {SvcName, {Wd,N,M}}),

    {okay, down} = ?WD_EVENT(Ref),

    %% Second transport process.
    ?EVENT({watchdog, Ref, _, {_, okay}, _}),
    reg(Ref, SvcName, 3),  %% answer 3 watchdogs then fall silent
    ?EVENT({watchdog, Ref, _, {_, down}, _}),

    %% Third transport process.
    ?EVENT({watchdog, Ref, _, {_, okay}, _}),
    reg(Ref, SvcName, 0),  %% disable outgoing DWA
    ?EVENT({watchdog, Ref, _, {_, down}, _}),

    ok.

log_wd({From, To} = T) ->
    ?LOG("~p -> ~p", [From, To]),
    T.

log_event(E) ->
    T = element(1,E),
    T == watchdog orelse ?LOG("~p", [T]),
    E.

%% recv_reopen/2

recv_reopen(connect, Ref) ->
    {down, reopen} = ?WD_EVENT(Ref),
    ?EVENT({reconnect, Ref, _});

recv_reopen(listen, Ref) ->
    {_, reopen} = ?WD_EVENT(Ref).

%% reg/3
%%
%% Lookup the pid of the transport process and publish a term for
%% message/3 to lookup.
reg(TRef, SvcName, T) ->
    TPid = tpid(TRef, diameter:service_info(SvcName, transport)),
    true = diameter_reg:add_new({?MODULE, TPid, T}).

%% tpid/2

tpid(Ref, [[{ref, Ref},
            {type, connect},
            {options, _},
            {watchdog, _},
            {peer, _},
            {apps, _},
            {caps, _},
            {port, [{owner, TPid} | _]}
            | _]]) ->
    TPid;

tpid(Ref, [[{ref, Ref},
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
%% # suspect/1
%% ===========================================================================

%% Configure transports to require a set number of watchdog timeouts
%% before moving from OKAY to SUSPECT.

suspect(_) ->
    [] = run([[abuse, [suspect, N]] || N <- [0,1,3]]).

suspect(Type, Fake, Ref, N)
  when is_reference(Ref) ->
    {SvcName, TRef}
        = start(Type, Ref, {?WD(10000), [{suspect, N}], Fake}),
    {initial, okay} = ?WD_EVENT(TRef),
    suspect(TRef, Fake, SvcName, N);

suspect(TRef, true, SvcName, _) ->
    reg(TRef, SvcName, 0),  %% disable outgoing DWA
    {okay, _} = ?WD_EVENT(TRef);

suspect(TRef, false, SvcName, 0) ->  %% SUSPECT disabled
    %% Wait 2+ watchdogs and see that only one watchdog has been sent.
    false = ?WD_EVENT(TRef, 28000),
    [1,0,0,0] = wd_counts(SvcName);

suspect(TRef, false, SvcName, N) ->
    %% Check that no watchdog transition takes place within N+
    %% watchdogs ...
    false = ?WD_EVENT(TRef, N*10000+8000),
    [1,0,0,0] = wd_counts(SvcName),
    %% ... but that the connection then becomes suspect ...
    {okay, suspect} = ?WD_EVENT(TRef, 10000),
    [1,0,0,0] = wd_counts(SvcName),
    %% ... and goes down.
    {suspect, down} = ?WD_EVENT(TRef, 18000),
    [1,0,0,0] = wd_counts(SvcName).

%% abuse/1

abuse(F) ->
    [] = run([[abuse, F, T] || T <- [listen, connect]]).

abuse(F, [_,_,_|_] = Args) ->
    ?LOG("~p", [Args]),
    apply(?MODULE, F, Args);

abuse([F|A], Test) ->
    Ref = make_ref(),
    [] = run([[abuse, F, [T, T == Test, Ref] ++ A]
              || T <- [listen, connect]]);

abuse(F, Test) ->
    abuse([F], Test).

%% ===========================================================================
%% # okay/1
%% ===========================================================================

%% Configure the number of watchdog exchanges before moving from
%% REOPEN to OKAY.

okay(_) ->
    [] = run([[abuse, [okay, N]] || N <- [0,2,3]]).

okay(Type, Fake, Ref, N)
  when is_reference(Ref) ->
    {SvcName, TRef}
        = start(Type, Ref, {?WD(10000),
                            [{okay, choose(Fake, 0, N)}],
                            Fake}),
    {initial, okay} = ?WD_EVENT(TRef),
    okay(TRef,
         Fake,
         SvcName,
         choose(Type == listen, initial, down),
         N).

okay(TRef, true, SvcName, Down, _) ->
    reg(TRef, SvcName, 0),  %% disable outgoing DWA
    {okay, down} = ?WD_EVENT(TRef),
    {Down, okay} = ?WD_EVENT(TRef),
    reg(TRef, SvcName, -1), %% enable outgoing DWA
    {okay, down} = ?WD_EVENT(TRef);

okay(TRef, false, SvcName, Down, N) ->
    {okay, suspect} = ?WD_EVENT(TRef),
    [1,0,0,0] = wd_counts(SvcName),
    {suspect, down} = ?WD_EVENT(TRef),
    ok(TRef, SvcName, Down, N).

ok(TRef, SvcName, Down, 0) ->
    %% Connection comes up without watchdog exchange.
    {Down, okay} = ?WD_EVENT(TRef),
    [1,0,0,0] = wd_counts(SvcName),
    %% Wait 2+ watchdog timeouts to see that the connection stays up
    %% and two watchdogs are exchanged.
    false = ?WD_EVENT(TRef, 28000),
    [3,0,0,2] = wd_counts(SvcName);

ok(TRef, SvcName, Down, N) ->
    %% Connection required watchdog exchange before reaching OKAY.
    {Down, reopen} = ?WD_EVENT(TRef),
    {reopen, okay} = ?WD_EVENT(TRef),
    %% One DWR was sent in moving to expect, plus N more to reopen the
    %% connection.
    N1 = N+1,
    [N1,0,0,N] = wd_counts(SvcName).

%% ===========================================================================

%% wd_counts/1

wd_counts(SvcName) ->
    [Info] = diameter:service_info(SvcName, transport),
    {_, Counters} = lists:keyfind(statistics, 1, Info),
    [proplists:get_value({{0,280,R}, D}, Counters, 0) || D <- [send,recv],
                                                         R <- [1,0]].

%% start/3

start(Type, Ref, T) ->
    Name = hostname(),
    true = diameter:subscribe(Name),
    ok = diameter:start_service(Name, [{monitor, self()} | ?SERVICE(Name)]),
    {ok, TRef} = diameter:add_transport(Name, {Type, opts(Type, Ref, T)}),
    true = diameter_reg:add_new({Type, Ref, Name}),
    {Name, TRef}.

opts(Type, Ref, {Timer, Config, Fake})
  when is_boolean(Fake) ->
    [{transport_module, diameter_tcp},
     {transport_config, mod(Fake) ++ [{ip, ?ADDR}, {port, 0}]
                                  ++ cfg(Type, Ref)},
     {watchdog_timer, Timer},
     {watchdog_config, Config}].

mod(B) ->
    [{message_cb, [fun message/3, capx]} || B].

cfg(listen, _) ->
    [];
cfg(connect, Ref) ->
    [{{_, _, SvcName}, _Pid}] = diameter_reg:wait({listen, Ref, '_'}),
    [[{ref, LRef} | _]] = diameter:service_info(SvcName, transport),
    [LP] = ?util:lport(tcp, LRef),
    [{raddr, ?ADDR}, {rport, LP}].

%% ===========================================================================

%% message/3

message(send, Bin, X) ->
    send(Bin, X);

message(recv, Bin, _) ->
    [Bin];

message(_, _, _) ->
    [].

%% send/2

%% First outgoing message from a new transport process is CER/CEA.
%% Remaining outgoing messages are either DWR or DWA.
send(Bin, capx) ->
    <<_:32, _:8, 257:24, _/binary>> = Bin,  %% assert on CER/CEA
    [Bin, fun message/3, init];

%% Outgoing DWR: fake reception of DWA. Use the fact that AVP values
%% are ignored. This is to ensure that the peer's watchdog state
%% transitions are only induced by responses to messages it sends.
send(<<_:32, 1:1, _:7, 280:24, _:32, EId:32, HId:32, _/binary>>, _) ->
    Pkt = #diameter_packet{header = #diameter_header{version = 1,
                                                     end_to_end_id = EId,
                                                     hop_by_hop_id = HId},
                           msg = ['DWA', {'Result-Code', 2001},
                                         {'Origin-Host', "XXX"},
                                         {'Origin-Realm', ?REALM}]},
    #diameter_packet{bin = Bin} = diameter_codec:encode(?BASE, Pkt),
    [recv, Bin];

%% First outgoing DWA.
send(Bin, init) ->
    [{{?MODULE, _, T}, _}] = diameter_reg:wait({?MODULE, self(), '_'}),
    send(Bin, T);

%% First transport process.
send(Bin, {SvcName, {_,_,_} = T}) ->
    [{'Origin-Host', _} = OH, {'Origin-Realm', _} = OR | _]
        = ?SERVICE(SvcName),
    putr(origin, [OH, OR]),
    send(Bin, T);

%% Discard DWA, failback after another timeout in the peer.
send(Bin, {Wd, 0 = No, Msg}) ->
    Origin = getr(origin),
    [{defer, ?ONE_WD(Wd), [msg(Msg, Bin, Origin)]}, fun message/3, No];

%% Send DWA while we're in the mood (aka 0 < N).
send(Bin, {Wd, N, Msg}) ->
    [Bin, fun message/3, {Wd, N-1, Msg}];

%% Discard DWA.
send(_Bin, 0 = No) ->
    [fun message/3, No];

%% Send DWA.
send(<<_:32, 0:1, _:7, 280:24, _/binary>> = DWA, N) ->
    [DWA, fun message/3, N-1].

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
             E:R:Stack ->
                 ?WARN("~p", [{A, E, R, Stack}]),
                 Stack
         end.

%% jitter/2

jitter(?WD(T), _) ->
    T;
jitter(T,D) ->
    T+D.

%% Generate a unique hostname for the faked peer.
hostname() ->
    ?util:unique_string().

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).
