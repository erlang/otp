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
%% Tests of the RFC3539 watchdog state machine as implemented by
%% module diameter_watchdog.
%%

-module(diameter_watchdog_SUITE).

-export([suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([reopen/1, reopen/4]).

-export([start/3, %% diameter_transport callback
         id/1,    %% jitter callback
         run/1]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_ct.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(BASE,    diameter_gen_base_rfc3588).
-define(APPL_ID, diameter_gen_base_rfc3588:id()).
-define(SUCCESS, 2001).  %% DIAMETER_SUCCESS

%% Addresses for the local and remote diameter nodes. The values don't
%% matter since we're faking transport.
-define(LOCALHOST, {127,0,0,1}).
-define(REMOTEHOST, {10,0,0,1}).

-define(CAPS, #diameter_caps{origin_host = "node.innan.com",
                             origin_realm = "innan.com",
                             host_ip_address = [?LOCALHOST],
                             vendor_id = 1022,
                             product_name = "remote",
                             auth_application_id = [?APPL_ID]}).

-define(APPL, #diameter_app{alias = ?MODULE,
                            dictionary = ?BASE,
                            module = [?MODULE],
                            init_state = now(),
                            id = ?APPL_ID,
                            mutable = false}).

%% Service record maintained by our faked service process.
-define(SERVICE, #diameter_service{pid = self(),
                                   capabilities = ?CAPS,
                                   applications = [?APPL]}).

%% Watchdog timer as a callback.
-define(WD(T), {?MODULE, id, [T]}).

%% Watchdog timers used by the testcases. Note that the short timeout
%% with random jitter is excluded since the reopen/1 isn't smart
%% enough to deal with it: see ONE_WD below.
-define(WD_TIMERS, [?WD(6000)
                    | [F_(T_) || T_ <- [10000, 20000, 30000],
                                 F_ <- [fun(T__) -> T__ end,
                                        fun(T__) -> ?WD(T__) end]]]).

%% Transport types.
-define(TRANSPORTS, [connect, accept]).

%% Message over the transport interface.
-define(TMSG(T), {diameter, T}).

%% Receive a message within a specified time.
-define(RECV(T, Timeout),
        receive T -> now()
        after Timeout -> ?ERROR({timeout, Timeout})
        end).

%% Receive a message in a given number of watchdogs, plus or minus
%% half. Note that the call to now_diff assumes left to right
%% evaluation order.
-define(RECV(T, N, WdL, WdH),
        [?ERROR({received, _Elapsed_, _LowerBound_, N, WdL})
         || _UpperBound_ <- [(N)*(WdH) + (WdH) div 2],
            _Elapsed_    <- [now_diff(now(), ?RECV(T, _UpperBound_))],
            _LowerBound_ <- [(N)*(WdL) - (WdL) div 2],
            _Elapsed_ =< _LowerBound_*1000]).

%% A timeout that ensures one watchdog. The ensure only one watchdog
%% requires (Wd + 2000) + 1000 < 2*(Wd - 2000) ==> 7000 < Wd for the
%% case with random jitter.
-define(ONE_WD(Wd), jitter(Wd,2000) + 1000).

%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 6}}].%% enough for 11 watchdogs @ 30 sec plus jitter

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

reopen(_) ->
    [] = ?util:run([{?MODULE, [run, [reopen, Wd, T, N, M]]}
                    || Wd <- ?WD_TIMERS,
                       T <- ?TRANSPORTS,
                       N <- [0,1,2],
                       M <- ['DWR', 'DWA', other]]).

reopen(Wd, Type, N, What) ->
    Ref = make_ref(),

    %% The maker of transport processes.
    TPid = start({N, Wd, What, Ref}),

    %% Act like diameter_service and start the watchdog process, which
    %% in turn starts a peer_fsm process, which in turn starts a
    %% transport process by way of start/3. Messages received by the
    %% testcase are those sent by diameter_watchdog to the service
    %% process (= process starting the watchdog).
    WPid1 = watchdog(Type, Ref, TPid, Wd),

    %% Low/high watchdog timeouts.
    WdL = jitter(Wd, -2000),
    WdH = jitter(Wd, 2000),

    %% Connection should come up immediately as a consequence of
    %% starting the watchdog process. In the accepting case this
    %% results in a new watchdog on a transport waiting for a new
    %% connection.
    ?RECV({connection_up, WPid1, _}, 1000),

    WPid2 = case Type of
                connect ->
                    WPid1;
                accept ->
                    watchdog(Type, Ref, TPid, Wd)
            end,

    %%   OKAY          Timer expires &      Failover()
    %%                 Pending              SetWatchdog()        SUSPECT
    %%
    %% Since our transport is replying to N DWR's before becoming
    %% silent, we should go down after N+2 watchdog_timer expirations:
    %% that is, after the first unanswered DWR. Knowing the min/max
    %% watchdog timeout values gives the time interval in which the
    %% down message is expected.
    ?RECV({connection_down, WPid1}, N+2, WdL, WdH),

    %%   SUSPECT       Receive DWA          Pending = FALSE
    %%                                      Failback()
    %%                                      SetWatchdog()        OKAY
    %%
    %%   SUSPECT       Receive non-DWA      Failback()
    %%                                      SetWatchdog()        OKAY
    %%
    %% The transport receives a message before the expiry of another
    %% watchdog to induce failback.
    ?RECV({connection_up, WPid1}, WdH),

    %%   OKAY          Timer expires &      SendWatchdog()
    %%                 !Pending             SetWatchdog()
    %%                                      Pending = TRUE       OKAY
    %%
    %%   OKAY          Timer expires &      Failover()
    %%                 Pending              SetWatchdog()        SUSPECT
    %%
    %% The transport is still not responding to watchdogs so the
    %% connection should go back down after either one or two watchdog
    %% expiries, depending on whether or not DWA restored the connection.
    F = choose(What == 'DWA', 2, 1),
    ?RECV({connection_down, WPid1}, F, WdL, WdH),

    %%   SUSPECT       Timer expires        CloseConnection()
    %%                                      SetWatchdog()        DOWN
    %%
    %%   DOWN          Timer expires        AttemptOpen()
    %%                                      SetWatchdog()        DOWN
    %%
    %% Our transport tells us when the fake connection is
    %% reestablished, which should happen after another couple of
    %% watchdog expiries, the first bringing the watchdog to state
    %% DOWN, the second triggering an attempt to reopen the
    %% connection.
    ?RECV({reopen, Ref}, 2, WdL, WdH),

    %%   DOWN          Connection up        NumDWA = 0
    %%                                      SendWatchdog()
    %%                                      SetWatchdog()
    %%                                      Pending = TRUE       REOPEN
    %%
    %%   REOPEN        Receive DWA &        Pending = FALSE
    %%                 NumDWA < 2           NumDWA++             REOPEN
    %%
    %%   REOPEN        Receive DWA &        Pending = FALSE
    %%                 NumDWA == 2          NumDWA++
    %%                                      Failback()           OKAY
    %%
    %% Now the watchdog should require three received DWA's before
    %% taking the connection back up. The first DWR is sent directly
    %% after capabilities exchange so it should take no more than two
    %% watchdog expiries.
    ?RECV({connection_up, WPid2, _}, 2, WdL, WdH).

%% ===========================================================================

%% Start the fake transport process. From diameter's point of view
%% it's started when diameter calls start/3. We start it before this
%% happens since we use the same fake transport each time diameter
%% calls start/3. The process lives and dies with the test case.
start(Config) ->
    Pid = self(),
    spawn(fun() -> loop(init(Pid, Config)) end).

%% Transport start from diameter. This may be called multiple times
%% depending on the testcase.
start({Type, _Ref}, #diameter_service{}, Pid) ->
    Ref = make_ref(),
    MRef = erlang:monitor(process, Pid),
    Pid ! {start, self(), Type, Ref},
    {Ref, TPid} = receive
                      {Ref, _} = T ->
                          T;
                      {'DOWN', MRef, process, _, _} = T ->
                          T
                  end,
    erlang:demonitor(MRef, [flush]),
    {ok, TPid}.

%% id/1

id(T) ->
    T.

%% ===========================================================================

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

%% run/1
%%
%% A more useful badmatch in case of failure.

run([F|A]) ->
    ok = try
             apply(?MODULE, F, A),
             ok
         catch
             E:R ->
                 {A, E, R, erlang:get_stacktrace()}
         end.

%% now_diff/2

now_diff(T1, T2) ->
    timer:now_diff(T2, T1).

%% jitter/2

jitter(?WD(T), _) ->
    T;
jitter(T,D) ->
    T+D.

%% watchdog/4
%%
%% Fake the call from diameter_service. The watchdog process will send
%% messages to the calling "service" process so our tests are that the
%% watchdog responds as expected.

watchdog(Type, Ref, TPid, Wd) ->
    Opts = [{transport_module, ?MODULE},
            {transport_config, TPid},
            {watchdog_timer, Wd}],
    monitor(diameter_watchdog:start({Type, Ref},
                                    {false, Opts, false, ?SERVICE})).

monitor(Pid) ->
    erlang:monitor(process, Pid),
    Pid.

%% ===========================================================================

%% Transport process implmentation. Fakes reception of messages by
%% sending fakes to the parent (peer fsm) process that called start/3.

-record(transport,
        {type,    %% connect | accept | manager
         parent,  %% pid() of peer_fsm/ervice process
         open = false,  %% done with capabilities exchange?
         config}).%% testcase-specific config

%% init/2

%% Testcase starting the manager.
init(SvcPid, {_,_,_,_} = Config) ->
    putr(peer, [{'Origin-Host', hostname() ++ ".utan.com"},
                {'Origin-Realm', "utan.com"}]),
    #transport{type = manager,
               parent = monitor(SvcPid),
               config = Config};

%% Manager starting a transport.
init(_, {Type, ParentPid, SvcPid, TwinPid, Peer, {N,_,_,_} = Config}) ->
    putr(peer, Peer),
    putr(service, SvcPid),
    putr(count, init(Type, ParentPid, TwinPid, N)),%% number of DWR's to answer
    #transport{type = Type,
               parent = monitor(ParentPid),
               config = Config}.

init(Type, ParentPid, undefined, N) ->
    connected(ParentPid, Type),
    N;
init(_, _, TPid, _) ->
    monitor(TPid),
    3.

%% Generate a unique hostname for the faked peer.
hostname() ->
    lists:flatten(io_lib:format("~p-~p-~p", tuple_to_list(now()))).

%% loop/1

loop(S) ->
    loop(msg(receive T -> T end, S)).

msg(T,S) ->
    case transition(T,S) of
        ok ->
            S;
        #transport{} = NS ->
            NS;
        {stop, Reason} ->
            x(Reason)
    end.

x(Reason) ->
    exit(Reason).

%% transition/2

%% Manager is being asked for a new transport process.
transition({start, Pid, Type, Ref}, #transport{type = manager,
                                               parent = SvcPid,
                                               config = Config}) ->
    TPid = start({Type, Pid, SvcPid, getr(transport), getr(peer), Config}),
    Pid ! {Ref, TPid},
    putr(transport, TPid),
    ok;

%% Peer fsm or testcase process has died.
transition({'DOWN', _, process, Pid, _} = T, #transport{parent = Pid}) ->
    {stop, T};

%% Twin transport process has gone down. In the connect case, the
%% transport isn't started until this happens in the first place so
%% connect immediately. In the accept case, fake the peer reconnecting
%% only after another watchdog expiry.
transition({'DOWN', _, process, _, _}, #transport{type = Type,
                                                  config = {_, Wd, _, _}}) ->
    Tmo = case Type of
              connect ->
                  0;
              accept ->
                  ?ONE_WD(Wd)
          end,
    erlang:send_after(Tmo, self(), reconnect),
    ok;

transition(reconnect, #transport{type = Type,
                                 parent = Pid,
                                 config = {_,_,_,Ref}}) ->
    getr(service) ! {reopen, Ref},
    connected(Pid, Type),
    ok;

%% Peer fsm process is sending CER: fake the peer's CEA.
transition(?TMSG({send, Bin}), #transport{type = connect,
                                          open = false,
                                          parent = Pid}
                               = S) ->
    {Code, Flags, _} = ?BASE:msg_header('CER'),
    <<_:32, Flags:8, Code:24, _:96, _/binary>> = Bin,
    Hdr = make_header(Bin),
    recv(Pid, {Hdr, make_cea()}),
    S#transport{open = true};

%% Peer fsm process is sending CEA.
transition(?TMSG({send, Bin}), #transport{type = accept,
                                          open = false}
                               = S) ->
    {Code, Flags, _} = ?BASE:msg_header('CEA'),
    <<_:32, Flags:8, Code:24, _:96, _/binary>> = Bin,
    S#transport{open = true};

%% Watchdog is sending DWR or DWA.
transition(?TMSG({send, Bin}), #transport{open = true} = S) ->
    {Code, _, _} = ?BASE:msg_header('DWR'),
    {Code, _, _} = ?BASE:msg_header('DWA'),
    <<_:32, R:1, 0:7, Code:24, _:96, _/binary>> = Bin,
    Hdr = make_header(Bin),
    dwa(1 == R, S, Hdr),
    ok;

%% We're telling ourselves to fake a received message.
transition({recv, Msg}, #transport{parent = Pid}) ->
    recv(Pid, Msg),
    ok;

%% We're telling ourselves to receive a message to induce failback.
transition(failback = T, #transport{parent = Pid}) ->
    recv(Pid, eraser(T)),
    ok.

make_header(Bin) ->
    #diameter_header{end_to_end_id = E,
                     hop_by_hop_id = H}
        = diameter_codec:decode_header(Bin),
    #diameter_header{end_to_end_id = E,
                     hop_by_hop_id = H}.

recv(Pid, Msg) ->
    Pid ! ?TMSG({recv, encode(Msg)}).

%% Replace the end-to-end/hop-by-hop identifiers with those from an
%% incoming request to which we're constructing a reply.
encode({Hdr, [_|_] = Msg}) ->
    #diameter_header{hop_by_hop_id = HBH,
                     end_to_end_id = E2E}
        = Hdr,
    #diameter_packet{bin = Bin} = diameter_codec:encode(?BASE, Msg),
    <<H:12/binary, _:64, T/binary>> = Bin,
    <<H/binary, HBH:32, E2E:32, T/binary>>;

encode([_|_] = Msg) ->
    #diameter_packet{bin = Bin} = diameter_codec:encode(?BASE, Msg),
    Bin.

connected(Pid, connect) ->
    Pid ! ?TMSG({self(), connected, make_ref()});
connected(Pid, accept) ->
    Pid ! ?TMSG({self(), connected}),
    recv(Pid, make_cer()).

make_cer() ->
    ['CER' | getr(peer)] ++ [{'Host-IP-Address', [?REMOTEHOST]},
                             {'Vendor-Id', 1028},
                             {'Product-Name', "Utan"},
                             {'Auth-Application-Id', [?APPL_ID]}].

make_cea() ->
    ['CER' | Rest] = make_cer(),
    ['CEA', {'Result-Code', ?SUCCESS} | Rest].

make_dwr() ->
    ['DWR' | getr(peer)].

make_dwa() ->
    ['DWR' | Rest] = make_dwr(),
    ['DWA', {'Result-Code', ?SUCCESS} | Rest].

dwa(false, _, _) ->  %% outgoing was DWA ...
    ok;
dwa(true, S, Hdr) -> %% ... or DWR
    dwa(getr(count), Hdr, S);

%% React to the DWR only after another watchdog expiry. We shouldn't
%% get another DWR while the answer is pending.
dwa(0, Hdr, #transport{config = {_, Wd, What, _}}) ->
    erlang:send_after(?ONE_WD(Wd), self(), failback),
    putr(failback, make_msg(What, Hdr)),
    eraser(count);

dwa(undefined, _, _) ->
    undefined = getr(failback),  %% ensure this is after failback
    ok;

%% Reply with DWA.
dwa(N, Hdr, #transport{parent = Pid}) ->
    putr(count, N-1),
    recv(Pid, {Hdr, make_dwa()}).

%% Answer to received DWR.
make_msg('DWA', Hdr) ->
    {Hdr, make_dwa()};

%% DWR from peer.
make_msg('DWR', _) ->
    make_dwr();

%% An unexpected answer is discarded after passing through the
%% watchdog state machine.
make_msg(other, _) ->
    ['RAA', {'Session-Id', diameter:session_id("abc")},
            {'Result-Code', 2001}
          | getr(peer)].

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).

eraser(Key) ->
    erase({?MODULE, Key}).
