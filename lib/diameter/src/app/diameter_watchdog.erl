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
%% This module implements (as a process) the state machine documented
%% in Appendix A of RFC 3539.
%%

-module(diameter_watchdog).
-behaviour(gen_server).

%% towards diameter_service
-export([start/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% diameter_watchdog_sup callback
-export([start_link/1]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

-define(DEFAULT_TW_INIT, 30000). %% RFC 3539 ch 3.4.1

-record(watchdog,
        {%% PCB - Peer Control Block; see RFC 3539, Appendix A
         status = initial :: initial | okay | suspect | down | reopen,
         pending = false  :: boolean(),
         tw :: 6000..16#FFFFFFFF | {module(), atom(), list()},
                                %% {M,F,A} -> integer() >= 0
         num_dwa = 0 :: -1 | non_neg_integer(),
                     %% number of DWAs received during reopen
         %% end PCB
         parent = self() :: pid(),
         transport       :: pid(),
         tref :: reference(), %% reference for current watchdog timer
         message_data}).   %% term passed into diameter_service with message

%% start/2

start({_,_} = Type, T) ->
    {ok, Pid} = diameter_watchdog_sup:start_child({Type, self(), T}),
    Pid.

start_link(T) ->
    {ok, _} = proc_lib:start_link(?MODULE,
                                  init,
                                  [T],
                                  infinity,
                                  diameter_lib:spawn_opts(server, [])).

%% ===========================================================================
%% ===========================================================================

%% init/1

init(T) ->
    proc_lib:init_ack({ok, self()}),
    gen_server:enter_loop(?MODULE, [], i(T)).

i({T, Pid, {ConnT, Opts, SvcName, #diameter_service{applications = Apps,
                                                    capabilities = Caps}
                                  = Svc}}) ->
    {M,S,U} = now(),
    random:seed(M,S,U),
    putr(restart, {T, Opts, Svc}),  %% save seeing it in trace
    putr(dwr, dwr(Caps)),           %%
    #watchdog{parent = monitor(Pid),
              transport = monitor(diameter_peer_fsm:start(T, Opts, Svc)),
              tw = proplists:get_value(watchdog_timer,
                                       Opts,
                                       ?DEFAULT_TW_INIT),
              message_data = {ConnT, SvcName, Apps}}.

%% handle_call/3

handle_call(_, _, State) ->
    {reply, nok, State}.

%% handle_cast/2

handle_cast(_, State) ->
    {noreply, State}.

%% handle_info/2

handle_info(T, State) ->
    case transition(T, State) of
        ok ->
            {noreply, State};
        #watchdog{status = X} = S ->
            ?LOGC(X =/= State#watchdog.status, transition, X),
            {noreply, S};
        stop ->
            ?LOG(stop, T),
            {stop, {shutdown, T}, State}
    end.

%% terminate/2

terminate(_, _) ->
    ok.

%% code_change/3

code_change(_, State, _) ->
    {ok, State}.

%% ===========================================================================
%% ===========================================================================

%% transition/2
%%
%% The state transitions documented here are extracted from RFC 3539,
%% the commentary is ours.

%% Service or watchdog is telling the watchdog of an accepting
%% transport to die after reconnect_timer expiry or reestablished
%% connection (in another transport process) respectively.
transition(close, #watchdog{status = down}) ->
    {{accept, _}, _, _} = getr(restart), %% assert
    stop;
transition(close, #watchdog{}) ->
    ok;

%% Service is asking for the peer to be taken down gracefully.
transition({shutdown, Pid}, #watchdog{parent = Pid,
                                      transport = undefined,
                                      status = S}) ->
    down = S,  %% sanity check
    stop;
transition({shutdown = T, Pid}, #watchdog{parent = Pid,
                                          transport = TPid}) ->
    TPid ! {T, self()},
    ok;

%% Parent process has died,
transition({'DOWN', _, process, Pid, _Reason},
           #watchdog{parent = Pid}) ->
    stop;

%% Transport has accepted a connection.
transition({accepted = T, TPid}, #watchdog{transport = TPid,
                                           parent = Pid}) ->
    Pid ! {T, self(), TPid},
    ok;

%% Transport is telling us that its impending death isn't failure.
transition({close, TPid, _Reason}, #watchdog{transport = TPid}) ->
    stop;

%%   STATE         Event                Actions              New State
%%   =====         ------               -------              ----------
%%   INITIAL       Connection up        SetWatchdog()        OKAY

%% By construction, the watchdog timer isn't set until we move into
%% state okay as the result of the Peer State Machine reaching the
%% Open state.
%%
%% If we're an acceptor then we may be resuming a connection that went
%% down in another acceptor process, in which case this is the
%% transition below, from down into reopen. That is, it's not until
%% we know the identity of the peer (ie. now) that we know that we're
%% in state down rather than initial.

transition({open, TPid, Hosts, T} = Open,
           #watchdog{transport = TPid,
                     status = initial,
                     parent = Pid}
           = S) ->
    case okay(getr(restart), Hosts) of
        okay ->
            open(Pid, {TPid, T}),
            set_watchdog(S#watchdog{status = okay});
        reopen ->
            transition(Open, S#watchdog{status = down})
    end;

%%   DOWN          Connection up        NumDWA = 0
%%                                      SendWatchdog()
%%                                      SetWatchdog()
%%                                      Pending = TRUE       REOPEN

transition({open = P, TPid, _Hosts, T},
           #watchdog{transport = TPid,
                     status = down}
           = S) ->
    %% Store the info we need to notify the parent to reopen the
    %% connection after the requisite DWA's are received, at which
    %% time we eraser(open).
    putr(P, {TPid, T}),
    set_watchdog(send_watchdog(S#watchdog{status = reopen,
                                          num_dwa = 0}));

%%   OKAY          Connection down      CloseConnection()
%%                                      Failover()
%%                                      SetWatchdog()        DOWN
%%   SUSPECT       Connection down      CloseConnection()
%%                                      SetWatchdog()        DOWN
%%   REOPEN        Connection down      CloseConnection()
%%                                      SetWatchdog()        DOWN

transition({'DOWN', _, process, TPid, _},
           #watchdog{transport = TPid,
                     status = initial}) ->
    stop;

transition({'DOWN', _, process, Pid, _},
           #watchdog{transport = Pid}
           = S) ->
    failover(S),
    close(S),
    set_watchdog(S#watchdog{status = down,
                            pending = false,
                            transport = undefined});
%% Any outstanding pending (or other messages from the transport) will
%% have arrived before 'DOWN' since the message comes from the same
%% process. Note that we could also get this message in the initial
%% state.

%% Incoming message.
transition({recv, TPid, Name, Pkt}, #watchdog{transport = TPid} = S) ->
    recv(Name, Pkt, S);

%% Current watchdog has timed out.
transition({timeout, TRef, tw}, #watchdog{tref = TRef} = S) ->
    set_watchdog(timeout(S));

%% Timer was canceled after message was already sent.
transition({timeout, _, tw}, #watchdog{}) ->
    ok;

%% State query.
transition({state, Pid}, #watchdog{status = S}) ->
    Pid ! {self(), S},
    ok.

%% ===========================================================================

monitor(Pid) ->
    erlang:monitor(process, Pid),
    Pid.

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).

eraser(Key) ->
    erase({?MODULE, Key}).

%% encode/1

encode(Msg) ->
    #diameter_packet{bin = Bin} = diameter_codec:encode(?BASE, Msg),
    Bin.

%% okay/2

okay({{accept, Ref}, _, _}, Hosts) ->
    T = {?MODULE, connection, Ref, Hosts},
    diameter_reg:add(T),
    okay(diameter_reg:match(T));
%% Register before matching so that at least one of two registering
%% processes will match the other. (Which can't happen as long as
%% diameter_peer_fsm guarantees at most one open connection to the same
%% peer.)

okay({{connect, _}, _, _}, _) ->
    okay.

%% The peer hasn't been connected recently ...
okay([{_,P}]) ->
    P = self(),  %% assert
    okay;

%% ... or it has.
okay(C) ->
    [_|_] = [P ! close || {_,P} <- C, self() /= P],
    reopen.

%% set_watchdog/1

set_watchdog(#watchdog{tw = TwInit,
                       tref = TRef}
             = S) ->
    cancel(TRef),
    S#watchdog{tref = erlang:start_timer(tw(TwInit), self(), tw)}.

cancel(undefined) ->
    ok;
cancel(TRef) ->
    erlang:cancel_timer(TRef).

tw(T)
  when is_integer(T), T >= 6000 ->
    T - 2000 + (random:uniform(4001) - 1); %% RFC3539 jitter of +/- 2 sec.
tw({M,F,A}) ->
    apply(M,F,A).

%% open/2

open(Pid, {_,_} = T) ->
    Pid ! {connection_up, self(), T}.

%% failover/1

failover(#watchdog{status = okay,
                   parent = Pid}) ->
    Pid ! {connection_down, self()};

failover(_) ->
    ok.

%% close/1

close(#watchdog{status = down}) ->
    ok;

close(#watchdog{parent = Pid}) ->
    {{T, _}, _, _} = getr(restart),
    T == accept andalso (Pid ! {close, self()}).

%% send_watchdog/1

send_watchdog(#watchdog{pending = false,
                        transport = TPid}
              = S) ->
    TPid ! {send, encode(getr(dwr))},
    ?LOG(send, 'DWR'),
    S#watchdog{pending = true}.

%% recv/3

recv(Name, Pkt, S) ->
    try rcv(Name, S) of
        #watchdog{} = NS ->
            rcv(Name, Pkt, S),
            NS
    catch
        throw: {?MODULE, throwaway, #watchdog{} = NS} ->
            NS
    end.

%% rcv/3

rcv(N, _, _)
  when N == 'CER';
       N == 'CEA';
       N == 'DWR';
       N == 'DWA';
       N == 'DPR';
       N == 'DPA' ->
    false;

rcv(_, Pkt, #watchdog{transport = TPid,
                      message_data = T}) ->
    diameter_service:receive_message(TPid, Pkt, T).

throwaway(S) ->
    throw({?MODULE, throwaway, S}).

%% rcv/2

%%   INITIAL       Receive DWA          Pending = FALSE
%%                                      Throwaway()          INITIAL
%%   INITIAL       Receive non-DWA      Throwaway()          INITIAL

rcv('DWA', #watchdog{status = initial} = S) ->
    throwaway(S#watchdog{pending = false});

rcv(_, #watchdog{status = initial} = S) ->
    throwaway(S);

%%   DOWN          Receive DWA          Pending = FALSE
%%                                      Throwaway()          DOWN
%%   DOWN          Receive non-DWA      Throwaway()          DOWN

rcv('DWA', #watchdog{status = down} = S) ->
    throwaway(S#watchdog{pending = false});

rcv(_, #watchdog{status = down} = S) ->
    throwaway(S);

%%   OKAY          Receive DWA          Pending = FALSE
%%                                      SetWatchdog()        OKAY
%%   OKAY          Receive non-DWA      SetWatchdog()        OKAY

rcv('DWA', #watchdog{status = okay} = S) ->
    set_watchdog(S#watchdog{pending = false});

rcv(_, #watchdog{status = okay} = S) ->
    set_watchdog(S);

%%   SUSPECT       Receive DWA          Pending = FALSE
%%                                      Failback()
%%                                      SetWatchdog()        OKAY
%%   SUSPECT       Receive non-DWA      Failback()
%%                                      SetWatchdog()        OKAY

rcv('DWA', #watchdog{status = suspect} = S) ->
    failback(S),
    set_watchdog(S#watchdog{status = okay,
                            pending = false});

rcv(_, #watchdog{status = suspect} = S) ->
    failback(S),
    set_watchdog(S#watchdog{status = okay});

%%   REOPEN        Receive DWA &        Pending = FALSE
%%                 NumDWA == 2          NumDWA++
%%                                      Failback()           OKAY

rcv('DWA', #watchdog{status = reopen,
                     num_dwa = 2 = N,
                     parent = Pid}
           = S) ->
    open(Pid, eraser(open)),
    S#watchdog{status = okay,
               num_dwa = N+1,
               pending = false};

%%   REOPEN        Receive DWA &        Pending = FALSE
%%                 NumDWA < 2           NumDWA++             REOPEN

rcv('DWA', #watchdog{status = reopen,
                     num_dwa = N}
           = S) ->
    S#watchdog{num_dwa = N+1,
               pending = false};

%%   REOPEN        Receive non-DWA      Throwaway()          REOPEN

rcv(_, #watchdog{status = reopen} = S) ->
    throwaway(S).

%% failback/1

failback(#watchdog{parent = Pid}) ->
    Pid ! {connection_up, self()}.

%% timeout/1
%%
%% The caller sets the watchdog on the return value.

%%   OKAY          Timer expires &      SendWatchdog()
%%                 !Pending             SetWatchdog()
%%                                      Pending = TRUE       OKAY
%%   REOPEN        Timer expires &      SendWatchdog()
%%                 !Pending             SetWatchdog()
%%                                      Pending = TRUE       REOPEN

timeout(#watchdog{status = T,
                  pending = false}
        = S)
  when T == okay;
       T == reopen ->
    send_watchdog(S);

%%   OKAY          Timer expires &      Failover()
%%                 Pending              SetWatchdog()        SUSPECT

timeout(#watchdog{status = okay,
                  pending = true}
  = S) ->
    failover(S),
    S#watchdog{status = suspect};

%%   SUSPECT       Timer expires        CloseConnection()
%%                                      SetWatchdog()        DOWN
%%   REOPEN        Timer expires &      CloseConnection()
%%                 Pending &            SetWatchdog()
%%                 NumDWA < 0                                DOWN

timeout(#watchdog{status = T,
                  pending = P,
                  num_dwa = N,
                  transport = TPid}
        = S)
  when T == suspect;
       T == reopen, P, N < 0 ->
    exit(TPid, shutdown),
    close(S),
    S#watchdog{status = down};

%%   REOPEN        Timer expires &      NumDWA = -1
%%                 Pending &            SetWatchdog()
%%                 NumDWA >= 0                               REOPEN

timeout(#watchdog{status = reopen,
                  pending = true,
                  num_dwa = N}
        = S)
  when 0 =< N ->
    S#watchdog{num_dwa = -1};

%%   DOWN          Timer expires        AttemptOpen()
%%                                      SetWatchdog()        DOWN
%%   INITIAL       Timer expires        AttemptOpen()
%%                                      SetWatchdog()        INITIAL

%% RFC 3539, 3.4.1:
%%
%%   [5] While the connection is in the closed state, the AAA client MUST
%%       NOT attempt to send further watchdog messages on the connection.
%%       However, after the connection is closed, the AAA client continues
%%       to periodically attempt to reopen the connection.
%%
%%       The AAA client SHOULD wait for the transport layer to report
%%       connection failure before attempting again, but MAY choose to
%%       bound this wait time by the watchdog interval, Tw.

%% Don't bound, restarting the peer process only when the previous
%% process has died. We only need to handle state down since we start
%% the first watchdog when transitioning out of initial.

timeout(#watchdog{status = down} = S) ->
    restart(S).

%% restart/1

restart(#watchdog{transport = undefined} = S) ->
    restart(getr(restart), S);
restart(S) ->
    S.

%% Only restart the transport in the connecting case. For an accepting
%% transport, we've registered the peer connection when leaving state
%% initial and this is used by a new accepting process to realize that
%% it's actually in state down rather then initial when receiving
%% notification of an open connection.

restart({{connect, _} = T, Opts, Svc}, #watchdog{parent = Pid} = S) ->
    Pid ! {reconnect, self()},
    S#watchdog{transport = monitor(diameter_peer_fsm:start(T, Opts, Svc))};
restart({{accept, _}, _, _}, S) ->
    S.
%% Don't currently use Opts/Svc in the accept case but having them in
%% the process dictionary is helpful if the process dies unexpectedly.

%% dwr/1

dwr(#diameter_caps{origin_host = OH,
                   origin_realm = OR,
                   origin_state_id = OSI}) ->
    ['DWR', {'Origin-Host', OH},
            {'Origin-Realm', OR},
            {'Origin-State-Id', OSI}].
