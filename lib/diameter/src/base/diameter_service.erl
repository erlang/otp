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
%% Implements the process that represents a service.
%%

-module(diameter_service).
-behaviour(gen_server).

-export([start/1,
         stop/1,
         start_transport/2,
         stop_transport/2,
         info/2,
         call/4]).

%% towards diameter_watchdog
-export([receive_message/3]).

%% service supervisor
-export([start_link/1]).

-export([subscribe/1,
         unsubscribe/1,
         subscriptions/1,
         subscriptions/0,
         services/0,
         services/1,
         whois/1]).

%% test/debug
-export([call_module/3,
         state/1,
         uptime/1]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Other callbacks.
-export([send/1]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% The states mirrored by peer_up/peer_down callbacks.
-define(STATE_UP,   up).
-define(STATE_DOWN, down).

-type op_state() :: ?STATE_UP
                  | ?STATE_DOWN.

%% The RFC 3539 watchdog states that are now maintained, albeit
%% along with the old up/down. okay = up, else down.
-define(WD_INITIAL, initial).
-define(WD_OKAY,    okay).
-define(WD_SUSPECT, suspect).
-define(WD_DOWN,    down).
-define(WD_REOPEN,  reopen).

-type wd_state() :: ?WD_INITIAL
                  | ?WD_OKAY
                  | ?WD_SUSPECT
                  | ?WD_DOWN
                  | ?WD_REOPEN.

-define(DEFAULT_TC,     30000).  %% RFC 3588 ch 2.1
-define(DEFAULT_TIMEOUT, 5000).  %% for outgoing requests
-define(RESTART_TC,      1000).  %% if restart was this recent

-define(RELAY, ?DIAMETER_DICT_RELAY).

%% Used to be able to swap this with anything else dict-like but now
%% rely on the fact that a service's #state{} record does not change
%% in storing in it ?STATE table and not always going through the
%% service process. In particular, rely on the fact that operations on
%% a ?Dict don't change the handle to it.
-define(Dict, diameter_dict).

%% Table containing outgoing requests for which a reply has yet to be
%% received.
-define(REQUEST_TABLE, diameter_request).

%% Maintains state in a table. In contrast to previously, a service's
%% stat is not constant and is accessed outside of the service
%% process.
-define(STATE_TABLE, ?MODULE).

%% The default sequence mask.
-define(NOMASK, {0,32}).

%% The default restrict_connections.
-define(RESTRICT, nodes).

%% Workaround for dialyzer's lack of understanding of match specs.
-type match(T)
   :: T | '_' | '$1' | '$2' | '$3' | '$4'.

%% State of service gen_server.
-record(state,
        {id = now(),
         service_name,  %% as passed to start_service/2, key in ?STATE_TABLE
         service :: #diameter_service{},
         peerT = ets_new(peers) :: ets:tid(),%% #peer{} at start_fsm
         connT = ets_new(conns) :: ets:tid(),%% #conn{} at connection_up/reopen
         shared_peers = ?Dict:new(),         %% Alias -> [{TPid, Caps}, ...]
         local_peers = ?Dict:new(),          %% Alias -> [{TPid, Caps}, ...]
         monitor = false :: false | pid(),   %% process to die with
         options
         :: [{sequence, diameter:sequence()}  %% sequence mask
             | {restrict_connections, diameter:restriction()}
             | {share_peers, boolean()}  %% broadcast peers to remote nodes?
             | {use_shared_peers, boolean()}]}).%% use broadcasted peers?
%% shared_peers reflects the peers broadcast from remote nodes. Note
%% that the state term itself doesn't change, which is relevant for
%% the stateless application callbacks since the state is retrieved
%% from ?STATE_TABLE from outside the service process. The pid in the
%% service record is used to determine whether or not we need to call
%% the process for a pick_peer callback.

%% Record representing a watchdog process as implemented by
%% diameter_watchdog. The term "peer" here is historical, made
%% especially confusing by the fact that a peer_ref() in the
%% documentation is the key of a #conn{} record, not a #peer{} record.
%% The name is also unfortunate given the meaning of peer in the
%% Diameter sense.
-record(peer,
        {pid  :: match(pid()),
         type :: match(connect | accept),
         ref  :: match(reference()),  %% key into diameter_config
         options :: match([diameter:transport_opt()]),%% from start_transport
         op_state = {?STATE_DOWN, ?WD_INITIAL}
                 :: match(op_state() | {op_state(), wd_state()}),
         started = now(),      %% at process start
         conn = false :: match(boolean() | pid())}).
                      %% true at accepted, pid() at connection_up or reopen

%% Record representing a peer process as implemented by
%% diameter_peer_fsm. The term "conn" is historical. Despite the name
%% here, comments refer to watchdog and peer processes, that are keys
%% in #peer{} and #conn{} records respectively. To add to the
%% confusion, a #request.transport is a peer process = key in a
%% #conn{} record. The actual transport process (that the peer process
%% knows about and that has a transport connection) isn't seen here.
-record(conn,
        {pid   :: pid(),
         apps  :: [{0..16#FFFFFFFF, diameter:app_alias()}], %% {Id, Alias}
         caps  :: #diameter_caps{},
         started = now(),  %% at process start
         peer  :: pid()}). %% key into peerT

%% Record stored in diameter_request for each outgoing request.
-record(request,
        {from,               %% arg 2 of handle_call/3
         handler    :: match(pid()), %% request process
         transport  :: match(pid()), %% peer process
         caps       :: match(#diameter_caps{}),
         app        :: match(diameter:app_alias()),%% #diameter_app.alias
         dictionary :: match(module()),            %% #diameter_app.dictionary
         module     :: match([module() | list()]), %% #diameter_app.module
         filter     :: match(diameter:peer_filter()),
         packet     :: match(#diameter_packet{})}).

%% Record call/4 options are parsed into.
-record(options,
        {filter = none  :: diameter:peer_filter(),
         extra = []     :: list(),
         timeout = ?DEFAULT_TIMEOUT :: 0..16#FFFFFFFF,
         detach = false :: boolean()}).

%%% ---------------------------------------------------------------------------
%%% # start(SvcName)
%%% ---------------------------------------------------------------------------

start(SvcName) ->
    diameter_service_sup:start_child(SvcName).

start_link(SvcName) ->
    Options = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(?MODULE, [SvcName], Options).
%% Put the arbitrary term SvcName in a list in case we ever want to
%% send more than this and need to distinguish old from new.

%%% ---------------------------------------------------------------------------
%%% # stop(SvcName)
%%% ---------------------------------------------------------------------------

stop(SvcName) ->
    case whois(SvcName) of
        undefined ->
            {error, not_started};
        Pid ->
            stop(call_service(Pid, stop), Pid)
    end.

stop(ok, Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, process, _, _} -> ok end;
stop(No, _) ->
    No.

%%% ---------------------------------------------------------------------------
%%% # start_transport(SvcName, {Ref, Type, Opts})
%%% ---------------------------------------------------------------------------

start_transport(SvcName, {_,_,_} = T) ->
    call_service_by_name(SvcName, {start, T}).

%%% ---------------------------------------------------------------------------
%%% # stop_transport(SvcName, Refs)
%%% ---------------------------------------------------------------------------

stop_transport(_, []) ->
    ok;
stop_transport(SvcName, [_|_] = Refs) ->
    call_service_by_name(SvcName, {stop, Refs}).

%%% ---------------------------------------------------------------------------
%%% # info(SvcName, Item)
%%% ---------------------------------------------------------------------------

info(SvcName, Item) ->
    case find_state(SvcName) of
        #state{} = S ->
            service_info(Item, S);
        false ->
            undefined
    end.

%%% ---------------------------------------------------------------------------
%%% # receive_message(TPid, Pkt, MessageData)
%%% ---------------------------------------------------------------------------

%% Handle an incoming Diameter message in the watchdog process. This
%% used to come through the service process but this avoids that
%% becoming a bottleneck.

receive_message(TPid, Pkt, T)
  when is_pid(TPid) ->
    #diameter_packet{header = #diameter_header{is_request = R}} = Pkt,
    recv(R, (not R) andalso lookup_request(Pkt, TPid), TPid, Pkt, T).

%% Incoming request ...
recv(true, false, TPid, Pkt, T) ->
    try
        spawn(fun() -> recv_request(TPid, Pkt, T) end)
    catch
        error: system_limit = E ->  %% discard
            ?LOG({error, E}, now())
    end;

%% ... answer to known request ...
recv(false, #request{from = {_, Ref}, handler = Pid} = Req, _, Pkt, _) ->
    Pid ! {answer, Ref, Req, Pkt};
%% Note that failover could have happened prior to this message being
%% received and triggering failback. That is, both a failover message
%% and answer may be on their way to the handler process. In the worst
%% case the request process gets notification of the failover and
%% sends to the alternate peer before an answer arrives, so it's
%% always the case that we can receive more than one answer after
%% failover. The first answer received by the request process wins,
%% any others are discarded.

%% ... or not.
recv(false, false, _, _, _) ->
    ok.

%%% ---------------------------------------------------------------------------
%%% # call(SvcName, App, Msg, Options)
%%% ---------------------------------------------------------------------------

call(SvcName, App, Msg, Options)
  when is_list(Options) ->
    Rec = make_options(Options),
    Ref = make_ref(),
    Caller = {self(), Ref},
    Fun = fun() -> exit({Ref, call(SvcName, App, Msg, Rec, Caller)}) end,
    try spawn_monitor(Fun) of
        {_, MRef} ->
            recv(MRef, Ref, Rec#options.detach, false)
    catch
        error: system_limit = E ->
            {error, E}
    end.

%% Don't rely on gen_server:call/3 for the timeout handling since it
%% makes no guarantees about not leaving a reply message in the
%% mailbox if we catch its exit at timeout. It currently *can* do so,
%% which is also undocumented.

recv(MRef, _, true, true) ->
    erlang:demonitor(MRef, [flush]),
    ok;

recv(MRef, Ref, Detach, Sent) ->
    receive
        Ref ->  %% send has been attempted
            recv(MRef, Ref, Detach, true);
        {'DOWN', MRef, process, _, Reason} ->
            call_rc(Reason, Ref, Sent)
    end.

%% call/5 has returned ...
call_rc({Ref, Ans}, Ref, _) ->
    Ans;

%% ... or not. In this case failure/encode are documented.
call_rc(_, _, Sent) ->
    {error, choose(Sent, failure, encode)}.

%% call/5
%%
%% In the process spawned for the outgoing request.

call(SvcName, App, Msg, Opts, Caller) ->
    c(find_state(SvcName), App, Msg, Opts, Caller).

c(#state{service_name = Svc, options = [{_, Mask} | _]} = S,
  App,
  Msg,
  Opts,
  Caller) ->
    case find_transport(App, Msg, Opts, S) of
        {_,_,_} = T ->
            send_request(T, Mask, Msg, Opts, Caller, Svc);
        false ->
            {error, no_connection};
        {error, _} = No ->
            No
    end;

c(false, _, _, _, _) ->
    {error, no_service}.

%% find_state/1

find_state(SvcName) ->
    fs(ets:lookup(?STATE_TABLE, SvcName)).

fs([#state{} = S]) ->
    S;

fs([S]) ->  %% inserted from old code
    upgrade(S);

fs([]) ->
    false.

%% make_options/1

make_options(Options) ->
    lists:foldl(fun mo/2, #options{}, Options).

mo({timeout, T}, Rec)
  when is_integer(T), 0 =< T ->
    Rec#options{timeout = T};

mo({filter, F}, #options{filter = none} = Rec) ->
    Rec#options{filter = F};
mo({filter, F}, #options{filter = {all, Fs}} = Rec) ->
    Rec#options{filter = {all, [F | Fs]}};
mo({filter, F}, #options{filter = F0} = Rec) ->
    Rec#options{filter = {all, [F0, F]}};

mo({extra, L}, #options{extra = X} = Rec)
  when is_list(L) ->
    Rec#options{extra = X ++ L};

mo(detach, Rec) ->
    Rec#options{detach = true};

mo(T, _) ->
    ?ERROR({invalid_option, T}).

%%% ---------------------------------------------------------------------------
%%% # subscribe(SvcName)
%%% # unsubscribe(SvcName)
%%% ---------------------------------------------------------------------------

subscribe(SvcName) ->
    diameter_reg:add({?MODULE, subscriber, SvcName}).

unsubscribe(SvcName) ->
    diameter_reg:del({?MODULE, subscriber, SvcName}).

subscriptions(Pat) ->
    pmap(diameter_reg:match({?MODULE, subscriber, Pat})).

subscriptions() ->
    subscriptions('_').

pmap(Props) ->
    lists:map(fun({{?MODULE, _, Name}, Pid}) -> {Name, Pid} end, Props).

%%% ---------------------------------------------------------------------------
%%% # services(Pattern)
%%% ---------------------------------------------------------------------------

services(Pat) ->
    pmap(diameter_reg:match({?MODULE, service, Pat})).

services() ->
    services('_').

whois(SvcName) ->
    case diameter_reg:match({?MODULE, service, SvcName}) of
        [{_, Pid}] ->
            Pid;
        [] ->
            undefined
    end.

%% ===========================================================================
%% ===========================================================================

state(Svc) ->
    call_service(Svc, state).

uptime(Svc) ->
    call_service(Svc, uptime).

%% call_module/3

call_module(Service, AppMod, Request) ->
    call_service(Service, {call_module, AppMod, Request}).

%%% ---------------------------------------------------------------------------
%%% # init([SvcName])
%%% ---------------------------------------------------------------------------

init([SvcName]) ->
    process_flag(trap_exit, true),  %% ensure terminate(shutdown, _)
    i(SvcName, diameter_reg:add_new({?MODULE, service, SvcName})).

i(SvcName, true) ->
    {ok, i(SvcName)};
i(_, false) ->
    {stop, {shutdown, already_started}}.

%%% ---------------------------------------------------------------------------
%%% # handle_call(Req, From, State)
%%% ---------------------------------------------------------------------------

handle_call(T, From, S)
  when not is_record(S, state) ->
    handle_call(T, From, upgrade(S));

handle_call(state, _, S) ->
    {reply, S, S};

handle_call(uptime, _, #state{id = T} = S) ->
    {reply, diameter_lib:now_diff(T), S};

%% Start a transport.
handle_call({start, {Ref, Type, Opts}}, _From, S) ->
    {reply, start(Ref, {Type, Opts}, S), S};

%% Stop transports.
handle_call({stop, Refs}, _From, S) ->
    shutdown(Refs, S),
    {reply, ok, S};

%% pick_peer with mutable state
handle_call({pick_peer, Local, Remote, App}, _From, S) ->
    #diameter_app{mutable = true} = App,  %% assert
    {reply, pick_peer(Local, Remote, self(), S#state.service_name, App), S};

handle_call({call_module, AppMod, Req}, From, S) ->
    call_module(AppMod, Req, From, S);

%% Call from old code.
handle_call({info, Item}, _From, S) ->
    {reply, service_info(Item, S), S};

handle_call(stop, _From, S) ->
    shutdown(service, S),
    {stop, normal, ok, S};
%% The server currently isn't guaranteed to be dead when the caller
%% gets the reply. We deal with this in the call to the server,
%% stating a monitor that waits for DOWN before returning.

%% Watchdog is asking for the sequence mask.
handle_call(sequence, _From, #state{options = [{_, Mask} | _]} = S) ->
    {reply, Mask, S};

%% Watchdog is asking for the nodes restriction.
handle_call(restriction, _From, #state{options = [_,_,_,{_,R} | _]} = S) ->
    {reply, R, S};

handle_call(Req, From, S) ->
    unexpected(handle_call, [Req, From], S),
    {reply, nok, S}.

%%% ---------------------------------------------------------------------------
%%% # handle_cast(Req, State)
%%% ---------------------------------------------------------------------------

handle_cast(Req, S) ->
    unexpected(handle_cast, [Req], S),
    {noreply, S}.

%%% ---------------------------------------------------------------------------
%%% # handle_info(Req, State)
%%% ---------------------------------------------------------------------------

handle_info(T, #state{} = S) ->
    case transition(T,S) of
        ok ->
            {noreply, S};
        {stop, Reason} ->
            {stop, {shutdown, Reason}, S}
    end;

handle_info(T, S) ->
    handle_info(T, upgrade(S)).

%% transition/2

%% Peer process is telling us to start a new accept process.
transition({accepted, Pid, TPid}, S) ->
    accepted(Pid, TPid, S),
    ok;

%% Peer process has a new open connection.
transition({connection_up, Pid, T}, S) ->
    connection_up(Pid, T, S),
    ok;

%% Watchdog has a new connection that will be opened after DW[RA]
%% exchange. This message was added long after connection_up, to
%% communicate the information as soon as it's available. Leave
%% connection_up as is it for now, duplicated information and all.
transition({reopen, Pid, T}, S) ->
    reopen(Pid, T, S),
    ok;

%% Watchdog has left state OKAY.
transition({connection_down, Pid}, S) ->
    connection_down(Pid, S),
    ok;

%% Watchdog has returned to state OKAY.
transition({connection_up, Pid}, S) ->
    connection_up(Pid, S),
    ok;

%% Accepting transport has lost connectivity.
transition({close, Pid}, S) ->
    close(Pid, S),
    ok;

%% Connecting transport is being restarted by watchdog.
transition({reconnect, Pid}, S) ->
    reconnect(Pid, S),
    ok;

%% Watchdog is sending notification of a state transition. Note that
%% the connection_up/down messages are pre-date this message and are
%% still used. A watchdog message will follow these and communicate
%% the same state as was set in handling connection_up/down.
transition({watchdog, Pid, {TPid, From, To}}, #state{service_name = SvcName,
                                                     peerT = PeerT}) ->
    #peer{ref = Ref, type = T, options = Opts, op_state = {OS,_}}
        = P
        = fetch(PeerT, Pid),
    insert(PeerT, P#peer{op_state = {OS, To}}),
    send_event(SvcName, {watchdog, Ref, TPid, {From, To}, {T, Opts}}),
    ok;
%% Death of a watchdog process (#peer.pid) results in the removal of
%% it's peer and any associated conn record when 'DOWN' is received
%% (after this) but the states will be {?STATE_UP, ?WD_DOWN} for a
%% short time. (No real problem since ?WD_* is only used in
%% service_info.) We set ?WD_OKAY as a consequence of connection_up
%% since we know a watchdog is coming. We can't set anything at
%% connection_down since we don't know if the subsequent watchdog
%% message will be ?WD_DOWN or ?WD_SUSPECT. We don't (yet) set
%% ?STATE_* as a consequence of a watchdog message since this requires
%% changing some of the matching on ?STATE_*.
%%
%% Death of a peer process process (#conn.pid, #peer.conn) results in
%% connection_down followed by watchdog ?WD_DOWN. The latter doesn't
%% result in the conn record being deleted since 'DOWN' from death of
%% its watchdog doesn't (yet) deal with the record having been
%% removed.

%% Monitor process has died. Just die with a reason that tells
%% diameter_config about the happening. If a cleaner shutdown is
%% required then someone should stop us.
transition({'DOWN', MRef, process, _, Reason}, #state{monitor = MRef}) ->
    {stop, {monitor, Reason}};

%% Local watchdog process has died.
transition({'DOWN', _, process, Pid, Reason}, S)
  when node(Pid) == node() ->
    peer_down(Pid, Reason, S),
    ok;

%% Remote service wants to know about shared peers.
transition({service, Pid}, S) ->
    share_peers(Pid, S),
    ok;

%% Remote service is communicating a shared peer.
transition({peer, TPid, Aliases, Caps}, S) ->
    remote_peer_up(TPid, Aliases, Caps, S),
    ok;

%% Remote peer process has died.
transition({'DOWN', _, process, TPid, _}, S) ->
    remote_peer_down(TPid, S),
    ok;

%% Restart after tc expiry.
transition({tc_timeout, T}, S) ->
    tc_timeout(T, S),
    ok;

%% Request process is telling us it may have missed a failover message
%% after a transport went down and the service process looked up
%% outstanding requests.
transition({failover, TRef, Seqs}, S) ->
    failover(TRef, Seqs, S),
    ok;

%% Ensure upgraded state is stored in state table.
transition(upgrade, _) ->
    ok;

transition(Req, S) ->
    unexpected(handle_info, [Req], S),
    ok.

%% upgrade/1

upgrade({state, Id, Svc, Name, Svc, PT, CT, SB, UB, SD, LD, MPid}) ->
    S = #state{id = Id,
               service_name = Name,
               service = Svc,
               peerT = PT,
               connT = CT,
               shared_peers = SD,
               local_peers = LD,
               monitor = MPid,
               options = [{sequence, ?NOMASK},
                          {share_peers, SB},
                          {use_shared_peers, UB},
                          {restrict_connections, ?RESTRICT}]},
    upgrade_insert(S),
    S.

upgrade_insert(#state{service = #diameter_service{pid = Pid}} = S) ->
    if Pid == self() ->
            ets:insert(?STATE_TABLE, S);
       true ->
            Pid ! upgrade
    end.

%%% ---------------------------------------------------------------------------
%%% # terminate(Reason, State)
%%% ---------------------------------------------------------------------------

terminate(Reason, #state{service_name = Name} = S) ->
    send_event(Name, stop),
    ets:delete(?STATE_TABLE, Name),
    shutdown == Reason  %% application shutdown
        andalso shutdown(application, S).

%%% ---------------------------------------------------------------------------
%%% # code_change(FromVsn, State, Extra)
%%% ---------------------------------------------------------------------------

code_change(FromVsn,
            #state{service_name = SvcName,
                   service = #diameter_service{applications = Apps}}
            = S,
            Extra) ->
    lists:foreach(fun(A) ->
                          code_change(FromVsn, SvcName, Extra, A)
                  end,
                  Apps),
    {ok, S}.

code_change(FromVsn, SvcName, Extra, #diameter_app{alias = Alias} = A) ->
    {ok, S} = cb(A, code_change, [FromVsn,
                                  mod_state(Alias),
                                  Extra,
                                  SvcName]),
    mod_state(Alias, S).

%% ===========================================================================
%% ===========================================================================

unexpected(F, A, #state{service_name = Name}) ->
    ?UNEXPECTED(F, A ++ [Name]).

cb([_|_] = M, F, A) ->
    eval(M, F, A);
cb(Rec, F, A) ->
    {_, M} = app(Rec),
    eval(M, F, A).

app(#request{app = A, module = M}) ->
    {A,M};
app(#diameter_app{alias = A, module = M}) ->
    {A,M}.

eval([M|X], F, A) ->
    apply(M, F, A ++ X).

%% Callback with state.

state_cb(#diameter_app{mutable = false, init_state = S}, {ModX, F, A}) ->
    eval(ModX, F, A ++ [S]);

state_cb(#diameter_app{mutable = true, alias = Alias}, {_,_,_} = MFA) ->
    state_cb(MFA, Alias);

state_cb({ModX,F,A}, Alias)
  when is_list(ModX) ->
    eval(ModX, F, A ++ [mod_state(Alias)]).

choose(true, X, _)  -> X;
choose(false, _, X) -> X.

ets_new(Tbl) ->
    ets:new(Tbl, [{keypos, 2}]).

insert(Tbl, Rec) ->
    ets:insert(Tbl, Rec),
    Rec.

%% Using the process dictionary for the callback state was initially
%% just a way to make what was horrendous trace (big state record and
%% much else everywhere) somewhat more readable. There's not as much
%% need for it now but it's no worse (except possibly that we don't
%% see the table identifier being passed around) than an ets table so
%% keep it.

mod_state(Alias) ->
    get({?MODULE, mod_state, Alias}).

mod_state(Alias, ModS) ->
    put({?MODULE, mod_state, Alias}, ModS).

%%% ---------------------------------------------------------------------------
%%% # shutdown/2
%%% ---------------------------------------------------------------------------

%% remove_transport: ask watchdogs to terminate their transport.
shutdown(Refs, #state{peerT = PeerT})
  when is_list(Refs) ->
    ets:foldl(fun(P,ok) -> sp(P, Refs), ok end, ok, PeerT);

%% application/service shutdown: ask transports to terminate themselves.
shutdown(Reason, #state{peerT = PeerT}) ->
    %% A transport might not be alive to receive the shutdown request
    %% but give those that are a chance to shutdown gracefully.
    shutdown(conn, Reason, PeerT),
    %% Kill the watchdogs explicitly in case there was no transport.
    shutdown(peer, Reason, PeerT).

%% sp/2

sp(#peer{ref = Ref, pid = Pid}, Refs) ->
    lists:member(Ref, Refs)
        andalso (Pid ! {shutdown, self()}).  %% 'DOWN' cleans up

%% shutdown/3

shutdown(Who, Reason, T) ->
    diameter_lib:wait(ets:foldl(fun(X,A) -> shutdown(Who, X, Reason, A) end,
                                [],
                                T)).

shutdown(conn = Who, #peer{op_state = {OS,_}} = P, Reason, Acc) ->
    shutdown(Who, P#peer{op_state = OS}, Reason, Acc);

shutdown(conn,
         #peer{pid = Pid, op_state = ?STATE_UP, conn = TPid},
         Reason,
         Acc) ->
    TPid ! {shutdown, Pid, Reason},
    [TPid | Acc];

shutdown(peer, #peer{pid = Pid}, _Reason, Acc)
  when is_pid(Pid) ->
    exit(Pid, shutdown),
    [Pid | Acc];

shutdown(_, #peer{}, _, Acc) ->
    Acc.

%%% ---------------------------------------------------------------------------
%%% # call_service/2
%%% ---------------------------------------------------------------------------

call_service(Pid, Req)
  when is_pid(Pid) ->
    cs(Pid, Req);
call_service(SvcName, Req) ->
    call_service_by_name(SvcName, Req).

call_service_by_name(SvcName, Req) ->
    cs(whois(SvcName), Req).

cs(Pid, Req)
  when is_pid(Pid) ->
    try
        gen_server:call(Pid, Req, infinity)
    catch
        E: Reason when E == exit ->
            {error, {E, Reason}}
    end;

cs(undefined, _) ->
    {error, no_service}.

%%% ---------------------------------------------------------------------------
%%% # i/1
%%%
%%% Output: #state{}
%%% ---------------------------------------------------------------------------

%% Intialize the state of a service gen_server.

i(SvcName) ->
    %% Split the config into a server state and a list of transports.
    {#state{} = S, CL} = lists:foldl(fun cfg_acc/2,
                                     {false, []},
                                     diameter_config:lookup(SvcName)),

    %% Publish the state in order to be able to access it outside of
    %% the service process. Originally table identifiers were only
    %% known to the service process but we now want to provide the
    %% option of application callbacks being 'stateless' in order to
    %% avoid having to go through a common process. (Eg. An agent that
    %% sends a request for every incoming request.)
    true = ets:insert_new(?STATE_TABLE, S),

    %% Start fsms for each transport.
    lists:foreach(fun(T) -> start_fsm(T,S) end, CL),

    init_shared(S),
    send_event(SvcName, start),
    S.

cfg_acc({SvcName, #diameter_service{applications = Apps} = Rec, Opts},
        {false, Acc}) ->
    lists:foreach(fun init_mod/1, Apps),
    S = #state{service_name = SvcName,
               service = Rec#diameter_service{pid = self()},
               monitor = mref(get_value(monitor, Opts)),
               options = service_options(Opts)},
    {S, Acc};

cfg_acc({_Ref, Type, _Opts} = T, {S, Acc})
  when Type == connect;
       Type == listen ->
    {S, [T | Acc]}.

service_options(Opts) ->
    [{sequence, proplists:get_value(sequence, Opts, ?NOMASK)},
     {share_peers, get_value(share_peers, Opts)},
     {use_shared_peers, get_value(use_shared_peers, Opts)},
     {restrict_connections, proplists:get_value(restrict_connections,
                                                Opts,
                                                ?RESTRICT)}].
%% The order of options is significant since we match against the list.

mref(false = No) ->
    No;
mref(P) ->
    erlang:monitor(process, P).

init_shared(#state{options = [_, _, {_, true} | _],
                   service_name = Svc}) ->
    diameter_peer:notify(Svc, {service, self()});
init_shared(#state{options = [_, _, {_, false} | _]}) ->
    ok.

init_mod(#diameter_app{alias = Alias,
                       init_state = S}) ->
    mod_state(Alias, S).

start_fsm({Ref, Type, Opts}, S) ->
    start(Ref, {Type, Opts}, S).

get_value(Key, Vs) ->
    {_, V} = lists:keyfind(Key, 1, Vs),
    V.

%%% ---------------------------------------------------------------------------
%%% # start/3
%%% ---------------------------------------------------------------------------

%% If the initial start/3 at service/transport start succeeds then
%% subsequent calls to start/4 on the same service will also succeed
%% since they involve the same call to merge_service/2. We merge here
%% rather than earlier since the service may not yet be configured
%% when the transport is configured.

start(Ref, {T, Opts}, S)
  when T == connect;
       T == listen ->
    try
        {ok, start(Ref, type(T), Opts, S)}
    catch
        ?FAILURE(Reason) ->
            {error, Reason}
    end.
%% TODO: don't actually raise any errors yet

%% There used to be a difference here between the handling of
%% configured listening and connecting transports but now we simply
%% tell the transport_module to start an accepting or connecting
%% process respectively, the transport implementation initiating
%% listening on a port as required.
type(listen)      -> accept;
type(accept)      -> listen;
type(connect = T) -> T.

%% start/4

start(Ref, Type, Opts, #state{peerT = PeerT,
                              connT = ConnT,
                              service_name = SvcName,
                              service = Svc})
  when Type == connect;
       Type == accept ->
    Pid = s(Type, Ref, {ConnT,
                        Opts,
                        SvcName,
                        merge_service(Opts, Svc)}),
    insert(PeerT, #peer{pid = Pid,
                        type = Type,
                        ref = Ref,
                        options = Opts}),
    Pid.

%% Note that the service record passed into the watchdog is the merged
%% record so that each watchdog may get a different record. This
%% record is what is passed back into application callbacks.

s(Type, Ref, T) ->
    case diameter_watchdog:start({Type, Ref}, T) of
        {_MRef, Pid} ->
            Pid;
        Pid when is_pid(Pid) ->  %% from old code
            erlang:monitor(process, Pid),
            Pid
    end.

%% merge_service/2

merge_service(Opts, Svc) ->
    lists:foldl(fun ms/2, Svc, Opts).

%% Limit the applications known to the fsm to those in the 'apps'
%% option. That this might be empty is checked by the fsm. It's not
%% checked at config-time since there's no requirement that the
%% service be configured first. (Which could be considered a bit odd.)
ms({applications, As}, #diameter_service{applications = Apps} = S)
  when is_list(As) ->
    S#diameter_service{applications
                       = [A || A <- Apps,
                               lists:member(A#diameter_app.alias, As)]};

%% The fact that all capabilities can be configured on the transports
%% means that the service doesn't necessarily represent a single
%% locally implemented Diameter peer as identified by Origin-Host: a
%% transport can configure its own Origin-Host. This means that the
%% service little more than a placeholder for default capabilities
%% plus a list of applications that individual transports can choose
%% to support (or not).
ms({capabilities, Opts}, #diameter_service{capabilities = Caps0} = Svc)
  when is_list(Opts) ->
    %% make_caps has already succeeded in diameter_config so it will succeed
    %% again here.
    {ok, Caps} = diameter_capx:make_caps(Caps0, Opts),
    Svc#diameter_service{capabilities = Caps};

ms(_, Svc) ->
    Svc.

%%% ---------------------------------------------------------------------------
%%% # accepted/3
%%% ---------------------------------------------------------------------------

accepted(Pid, _TPid, #state{peerT = PeerT} = S) ->
    #peer{ref = Ref, type = accept = T, conn = false, options = Opts}
        = P
        = fetch(PeerT, Pid),
    insert(PeerT, P#peer{conn = true}),  %% mark replacement as started
    start(Ref, T, Opts, S).              %% start new watchdog

fetch(Tid, Key) ->
    [T] = ets:lookup(Tid, Key),
    case T of
        #peer{op_state = ?STATE_UP} = P ->
            P#peer{op_state = {?STATE_UP, ?WD_OKAY}};
        #peer{op_state = ?STATE_DOWN} = P ->
            P#peer{op_state = {?STATE_DOWN, ?WD_DOWN}};
        _ ->
            T
    end.

%%% ---------------------------------------------------------------------------
%%% # connection_up/3
%%% ---------------------------------------------------------------------------

%% Watchdog process has reached state OKAY.

connection_up(Pid, {TPid, {Caps, SApps, Pkt}}, #state{peerT = PeerT,
                                                      connT = ConnT}
                                               = S) ->
    P = fetch(PeerT, Pid),
    C = #conn{pid = TPid,
              apps = SApps,
              caps = Caps,
              peer = Pid},

    insert(ConnT, C),
    connection_up([Pkt], P#peer{conn = TPid}, C, S).

%%% ---------------------------------------------------------------------------
%%% # reopen/3
%%% ---------------------------------------------------------------------------

%% Note that this connection_up/3 rewrites the same #conn{} now
%% written here. Both do so in case reopen has not happened in old
%% code.

reopen(Pid, {TPid, {Caps, SApps, _Pkt}}, #state{peerT = PeerT,
                                                connT = ConnT}) ->
    P = fetch(PeerT, Pid),
    C = #conn{pid = TPid,
              apps = SApps,
              caps = Caps,
              peer = Pid},

    insert(ConnT, C),
    #peer{op_state = {?STATE_DOWN, _}}
        = P,
    insert(PeerT, P#peer{op_state = {?STATE_DOWN, ?WD_REOPEN},
                         conn = TPid}).

%%% ---------------------------------------------------------------------------
%%% # connection_up/2
%%% ---------------------------------------------------------------------------

%% Peer process has transitioned back into the open state. Note that there
%% has been no new capabilties exchange in this case.

connection_up(Pid, #state{peerT = PeerT,
                          connT = ConnT}
                   = S) ->
    #peer{conn = TPid} = P = fetch(PeerT, Pid),
    C = fetch(ConnT, TPid),
    connection_up([], P, C, S).

%% connection_up/4

connection_up(T, P, C, #state{peerT = PeerT,
                              local_peers = LDict,
                              service_name = SvcName,
                              service
                              = #diameter_service{applications = Apps}}
                       = S) ->
    #peer{conn = TPid, op_state = {?STATE_DOWN, _}}
        = P,
    #conn{apps = SApps, caps = Caps}
        = C,

    insert(PeerT, P#peer{op_state = {?STATE_UP, ?WD_OKAY}}),

    request_peer_up(TPid),
    insert_local_peer(SApps, {{TPid, Caps}, {SvcName, Apps}}, LDict),
    report_status(up, P, C, S, T).

insert_local_peer(SApps, T, LDict) ->
    lists:foldl(fun(A,D) -> ilp(A, T, D) end, LDict, SApps).

ilp({Id, Alias}, {TC, SA}, LDict) ->
    init_conn(Id, Alias, TC, SA),
    ?Dict:append(Alias, TC, LDict).

init_conn(Id, Alias, {TPid, _} = TC, {SvcName, Apps}) ->
    #diameter_app{module = ModX,
                  id = Id}  %% assert
        = find_app(Alias, Apps),

    peer_cb({ModX, peer_up, [SvcName, TC]}, Alias)
        orelse exit(TPid, kill).  %% fake transport failure

%% find_app/2

find_app(Alias, Apps) ->
    case lists:keyfind(Alias, #diameter_app.alias, Apps) of
        #diameter_app{options = E} = A when is_atom(E) ->  %% upgrade
            A#diameter_app{options = [{answer_errors, E}]};
        A ->
            A
    end.

%% Don't bring down the service (and all associated connections)
%% regardless of what happens.
peer_cb(MFA, Alias) ->
    try state_cb(MFA, Alias) of
        ModS ->
            mod_state(Alias, ModS),
            true
    catch
        E:R ->
            diameter_lib:error_report({failure, {E, R, Alias, ?STACK}}, MFA),
            false
    end.

%%% ---------------------------------------------------------------------------
%%% # connection_down/2
%%% ---------------------------------------------------------------------------

%% Watchdog has transitioned out of state OKAY.

connection_down(Pid, #state{peerT = PeerT,
                            connT = ConnT}
                     = S) ->
    #peer{op_state = {?STATE_UP, WS},  %% assert
          conn = TPid}
        = P
        = fetch(PeerT, Pid),

    C = fetch(ConnT, TPid),
    insert(PeerT, P#peer{op_state = {?STATE_DOWN, WS}}),
    connection_down(P,C,S).

%% connection_down/3

connection_down(#peer{op_state = {?STATE_DOWN, _}}, _, _) ->
    ok;

connection_down(#peer{conn = TPid,
                      op_state = {?STATE_UP, _}}
                = P,
                #conn{caps = Caps,
                      apps = SApps}
                = C,
                #state{service_name = SvcName,
                       service = #diameter_service{applications = Apps},
                       local_peers = LDict}
                = S) ->
    report_status(down, P, C, S, []),
    remove_local_peer(SApps, {{TPid, Caps}, {SvcName, Apps}}, LDict),
    request_peer_down(TPid, S).

remove_local_peer(SApps, T, LDict) ->
    lists:foldl(fun(A,D) -> rlp(A, T, D) end, LDict, SApps).

rlp({Id, Alias}, {TC, SA}, LDict) ->
    L = ?Dict:fetch(Alias, LDict),
    down_conn(Id, Alias, TC, SA),
    ?Dict:store(Alias, lists:delete(TC, L), LDict).

down_conn(Id, Alias, TC, {SvcName, Apps}) ->
    #diameter_app{module = ModX,
                  id = Id}  %% assert
        = find_app(Alias, Apps),

    peer_cb({ModX, peer_down, [SvcName, TC]}, Alias).

%%% ---------------------------------------------------------------------------
%%% # peer_down/3
%%% ---------------------------------------------------------------------------

%% Watchdog process has died.

peer_down(Pid, Reason, #state{peerT = PeerT} = S) ->
    P = fetch(PeerT, Pid),
    ets:delete_object(PeerT, P),
    closed(Reason, P, S),
    restart(P,S),
    peer_down(P,S).

%% Send an event at connection establishment failure.
closed({shutdown, {close, _TPid, Reason}},
       #peer{op_state = {?STATE_DOWN, _},
             ref = Ref,
             type = Type,
             options = Opts},
       #state{service_name = SvcName}) ->
    send_event(SvcName, {closed, Ref, Reason, {type(Type), Opts}});
closed(_, _, _) ->
    ok.

%% The watchdog has never reached OKAY ...
peer_down(#peer{conn = B}, _)
  when is_boolean(B) ->
    ok;

%% ... or maybe it has.
peer_down(#peer{conn = TPid} = P, #state{connT = ConnT} = S) ->
    #conn{} = C = fetch(ConnT, TPid),
    ets:delete_object(ConnT, C),
    connection_down(P,C,S).

%% restart/2

restart(P,S) ->
    q_restart(restart(P), S).

%% restart/1

%% Always try to reconnect.
restart(#peer{ref = Ref,
              type = connect = T,
              options = Opts,
              started = Time}) ->
    {Time, {Ref, T, Opts}};

%% Transport connection hasn't yet been accepted ...
restart(#peer{ref = Ref,
              type = accept = T,
              options = Opts,
              conn = false,
              started = Time}) ->
    {Time, {Ref, T, Opts}};

%% ... or it has: a replacement has already been spawned.
restart(#peer{type = accept}) ->
    false.

%% q_restart/2

%% Start the reconnect timer.
q_restart({Time, {_Ref, Type, Opts} = T}, S) ->
    start_tc(tc(Time, default_tc(Type, Opts)), T, S);
q_restart(false, _) ->
    ok.

%% RFC 3588, 2.1:
%%
%%   When no transport connection exists with a peer, an attempt to
%%   connect SHOULD be periodically made.  This behavior is handled via
%%   the Tc timer, whose recommended value is 30 seconds.  There are
%%   certain exceptions to this rule, such as when a peer has terminated
%%   the transport connection stating that it does not wish to
%%   communicate.

default_tc(connect, Opts) ->
    proplists:get_value(reconnect_timer, Opts, ?DEFAULT_TC);
default_tc(accept, _) ->
    0.

%% Bound tc below if the watchdog was restarted recently to avoid
%% continuous restarted in case of faulty config or other problems.
tc(Time, Tc) ->
    choose(Tc > ?RESTART_TC
             orelse timer:now_diff(now(), Time) > 1000*?RESTART_TC,
           Tc,
           ?RESTART_TC).

start_tc(0, T, S) ->
    tc_timeout(T, S);
start_tc(Tc, T, _) ->
    erlang:send_after(Tc, self(), {tc_timeout, T}).

%% tc_timeout/2

tc_timeout({Ref, _Type, _Opts} = T, #state{service_name = SvcName} = S) ->
    tc(diameter_config:have_transport(SvcName, Ref), T, S).

tc(true, {Ref, Type, Opts}, #state{service_name = SvcName}
                            = S) ->
    send_event(SvcName, {reconnect, Ref, Opts}),
    start(Ref, Type, Opts, S);
tc(false = No, _, _) ->  %% removed
    No.

%%% ---------------------------------------------------------------------------
%%% # close/2
%%% ---------------------------------------------------------------------------

%% The watchdog doesn't start a new fsm in the accept case, it
%% simply stays alive until someone tells it to die in order for
%% another watchdog to be able to detect that it should transition
%% from initial into reopen rather than okay. That someone is either
%% the accepting watchdog upon reception of a CER from the previously
%% connected peer, or us after reconnect_timer timeout.

close(Pid, #state{service_name = SvcName,
                  peerT = PeerT}) ->
    #peer{pid = Pid,
          type = accept,
          ref = Ref,
          options = Opts}
        = fetch(PeerT, Pid),

    c(Pid, diameter_config:have_transport(SvcName, Ref), Opts).

%% Tell watchdog to (maybe) die later ...
c(Pid, true, Opts) ->
    Tc = proplists:get_value(reconnect_timer, Opts, 2*?DEFAULT_TC),
    erlang:send_after(Tc, Pid, close);

%% ... or now.
c(Pid, false, _Opts) ->
    Pid ! close.

%% The RFC's only document the behaviour of Tc, our reconnect_timer,
%% for the establishment of connections but we also give
%% reconnect_timer semantics for a listener, being the time within
%% which a new connection attempt is expected of a connecting peer.
%% The value should be greater than the peer's Tc + jitter.

%%% ---------------------------------------------------------------------------
%%% # reconnect/2
%%% ---------------------------------------------------------------------------

reconnect(Pid, #state{service_name = SvcName,
                      peerT = PeerT}) ->
    #peer{ref = Ref,
          type = connect,
          options = Opts}
        = fetch(PeerT, Pid),
    send_event(SvcName, {reconnect, Ref, Opts}).

%%% ---------------------------------------------------------------------------
%%% # call_module/4
%%% ---------------------------------------------------------------------------

%% Backwards compatibility and never documented/advertised. May be
%% removed.

call_module(Mod, Req, From, #state{service
                                   = #diameter_service{applications = Apps},
                                   service_name = Svc}
                            = S) ->
    case cm([A || A <- Apps, Mod == hd(A#diameter_app.module)],
            Req,
            From,
            Svc)
    of
        {reply = T, RC} ->
            {T, RC, S};
        noreply = T ->
            {T, S};
        Reason ->
            {reply, {error, Reason}, S}
    end.

cm([#diameter_app{module = ModX, alias = Alias}], Req, From, Svc) ->
    MFA = {ModX, handle_call, [Req, From, Svc]},

    try state_cb(MFA, Alias) of
        {noreply = T, ModS} ->
            mod_state(Alias, ModS),
            T;
        {reply = T, RC, ModS} ->
            mod_state(Alias, ModS),
            {T, RC};
        T ->
            diameter_lib:error_report({invalid, T}, MFA),
            invalid
    catch
        E: Reason ->
            diameter_lib:error_report({failure, {E, Reason, ?STACK}}, MFA),
            failure
    end;

cm([], _, _, _) ->
    unknown;

cm([_,_|_], _, _, _) ->
    multiple.

%%% ---------------------------------------------------------------------------
%%% # send_request/6
%%% ---------------------------------------------------------------------------

%% Send an outgoing request in its dedicated process.
%%
%% Note that both encode of the outgoing request and of the received
%% answer happens in this process. It's also this process that replies
%% to the caller. The service process only handles the state-retaining
%% callbacks.
%%
%% The mod field of the #diameter_app{} here includes any extra
%% arguments passed to diameter:call/2.

send_request({TPid, Caps, App} = T, Mask, Msg, Opts, Caller, SvcName) ->
    #diameter_app{module = ModX}
        = App,

    Pkt = make_prepare_packet(Mask, Msg),

    send_req(cb(ModX, prepare_request, [Pkt, SvcName, {TPid, Caps}]),
             Pkt,
             T,
             Opts,
             Caller,
             SvcName,
             []).

send_req({send, P}, Pkt, T, Opts, Caller, SvcName, Fs) ->
    send_req(make_request_packet(P, Pkt), T, Opts, Caller, SvcName, Fs);

send_req({discard, Reason} , _, _, _, _, _, _) ->
    {error, Reason};

send_req(discard, _, _, _, _, _, _) ->
    {error, discarded};

send_req({eval_packet, RC, F}, Pkt, T, Opts, Caller, SvcName, Fs) ->
    send_req(RC, Pkt, T, Opts, Caller, SvcName, [F|Fs]);

send_req(E, _, {_, _, App}, _, _, _, _) ->
    ?ERROR({invalid_return, prepare_request, App, E}).

%% make_prepare_packet/2
%%
%% Turn an outgoing request as passed to call/4 into a diameter_packet
%% record in preparation for a prepare_request callback.

make_prepare_packet(_, Bin)
  when is_binary(Bin) ->
    #diameter_packet{header = diameter_codec:decode_header(Bin),
                     bin = Bin};

make_prepare_packet(Mask, #diameter_packet{msg = [#diameter_header{} = Hdr
                                                  | Avps]}
                          = Pkt) ->
    Pkt#diameter_packet{msg = [make_prepare_header(Mask, Hdr) | Avps]};

make_prepare_packet(Mask, #diameter_packet{header = Hdr} = Pkt) ->
    Pkt#diameter_packet{header = make_prepare_header(Mask, Hdr)};

make_prepare_packet(Mask, Msg) ->
    make_prepare_packet(Mask, #diameter_packet{msg = Msg}).

%% make_prepare_header/1

make_prepare_header(Mask, undefined) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(#diameter_header{end_to_end_id = Seq,
                                         hop_by_hop_id = Seq});

make_prepare_header(Mask, #diameter_header{end_to_end_id = undefined,
                                           hop_by_hop_id = undefined}) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(#diameter_header{end_to_end_id = Seq,
                                         hop_by_hop_id = Seq});

make_prepare_header(Mask, #diameter_header{end_to_end_id = undefined} = H) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(H#diameter_header{end_to_end_id = Seq});

make_prepare_header(Mask, #diameter_header{hop_by_hop_id = undefined} = H) ->
    Seq = diameter_session:sequence(Mask),
    make_prepare_header(H#diameter_header{hop_by_hop_id = Seq});

make_prepare_header(_, Hdr) ->
    make_prepare_header(Hdr).

%% make_prepare_header/1

make_prepare_header(#diameter_header{version = undefined} = Hdr) ->
    make_prepare_header(Hdr#diameter_header{version = ?DIAMETER_VERSION});

make_prepare_header(#diameter_header{} = Hdr) ->
    Hdr;

make_prepare_header(T) ->
    ?ERROR({invalid_header, T}).

%% make_request_packet/2
%%
%% Reconstruct a diameter_packet from the return value of
%% prepare_request or prepare_retransmit callback.

make_request_packet(Bin, _)
  when is_binary(Bin) ->
    make_prepare_packet(false, Bin);

make_request_packet(#diameter_packet{msg = [#diameter_header{} | _]}
                    = Pkt,
                    _) ->
    Pkt;

%% Returning a diameter_packet with no header from a prepare_request
%% or prepare_retransmit callback retains the header passed into it.
%% This is primarily so that the end to end and hop by hop identifiers
%% are retained.
make_request_packet(#diameter_packet{header = Hdr} = Pkt,
                    #diameter_packet{header = Hdr0}) ->
    Pkt#diameter_packet{header = fold_record(Hdr0, Hdr)};

make_request_packet(Msg, Pkt) ->
    Pkt#diameter_packet{msg = Msg}.

%% fold_record/2

fold_record(undefined, R) ->
    R;
fold_record(Rec, R) ->
    diameter_lib:fold_tuple(2, Rec, R).

%% send_req/6

send_req(Pkt, {TPid, Caps, App}, Opts, Caller, SvcName, Fs) ->
    #diameter_app{alias = Alias,
                  dictionary = Dict,
                  module = ModX,
                  options = [{answer_errors, AE} | _]}
        = App,

    EPkt = encode(Dict, Pkt, Fs),

    #options{filter = Filter,
             timeout = Timeout}
        = Opts,

    Req = #request{packet = Pkt,
                   from = Caller,
                   handler = self(),
                   transport = TPid,
                   caps = Caps,
                   app = Alias,
                   filter = Filter,
                   dictionary = Dict,
                   module = ModX},

    try
        TRef = send_request(TPid, EPkt, Req, Timeout),
        ack(Caller),
        handle_answer(SvcName, AE, recv_answer(Timeout, SvcName, {TRef, Req}))
    after
        erase_request(EPkt)
    end.

%% Tell caller a send has been attempted.
ack({Pid, Ref}) ->
    Pid ! Ref.

%% recv_answer/3

recv_answer(Timeout,
            SvcName,
            {TRef, #request{from = {_, Ref}, packet = RPkt} = Req}
            = T) ->

    %% Matching on TRef below ensures we ignore messages that pertain
    %% to a previous transport prior to failover. The answer message
    %% includes the #request{} since it's not necessarily Req; that
    %% is, from the last peer to which we've transmitted.

    receive
        {answer = A, Ref, Rq, Pkt} ->     %% Answer from peer
            {A, Rq, Pkt};
        {timeout = Reason, TRef, _} ->    %% No timely reply
            {error, Req, Reason};
        {failover = Reason, TRef, false} ->  %% No alternate peer
            {error, Req, Reason};
        {failover, TRef, Transport} ->    %% Resend to alternate peer
            try_retransmit(Timeout, SvcName, Req, Transport);
        {failover, TRef} ->  %% May have missed failover notification
            Seqs = diameter_codec:sequence_numbers(RPkt),
            Pid  = whois(SvcName),
            is_pid(Pid) andalso (Pid ! {failover, TRef, Seqs}),
            recv_answer(Timeout, SvcName, T)
    end.
%% Note that failover starts a new timer and that expiry of an old
%% timer value is ignored. This means that an answer could be accepted
%% from a peer after timeout in the case of failover.

try_retransmit(Timeout, SvcName, Req, Transport) ->
    try retransmit(Transport, Req, SvcName, Timeout) of
        T -> recv_answer(Timeout, SvcName, T)
    catch
        ?FAILURE(Reason) -> {error, Req, Reason}
    end.

%% handle_error/3

handle_error(Req, Reason, SvcName) ->
    #request{module = ModX,
             packet = Pkt,
             transport = TPid,
             caps = Caps}
        = Req,
    cb(ModX, handle_error, [Reason, msg(Pkt), SvcName, {TPid, Caps}]).

msg(#diameter_packet{msg = undefined, bin = Bin}) ->
    Bin;
msg(#diameter_packet{msg = Msg}) ->
    Msg.

%% encode/3

encode(Dict, Pkt, Fs) ->
    P = encode(Dict, Pkt),
    eval_packet(P, Fs),
    P.

%% encode/2

%% Note that prepare_request can return a diameter_packet containing
%% header or transport_data. Even allow the returned record to contain
%% an encoded binary. This isn't the usual case but could some in
%% handy, for test at least. (For example, to send garbage.)

%% The normal case: encode the returned message.
encode(Dict, #diameter_packet{msg = Msg, bin = undefined} = Pkt) ->
    D = pick_dictionary([Dict, ?BASE], Msg),
    diameter_codec:encode(D, Pkt);

%% Callback has returned an encoded binary: just send.
encode(_, #diameter_packet{} = Pkt) ->
    Pkt.

%% pick_dictionary/2

%% Pick the first dictionary that declares the application id in the
%% specified header.
pick_dictionary(Ds, [#diameter_header{application_id = Id} | _]) ->
    pd(Ds, fun(D) -> Id = D:id() end);

%% Pick the first dictionary that knows the specified message name.
pick_dictionary(Ds, [MsgName|_]) ->
    pd(Ds, fun(D) -> D:msg2rec(MsgName) end);

%% Pick the first dictionary that knows the name of the specified
%% message record.
pick_dictionary(Ds, Rec) ->
    Name = element(1, Rec),
    pd(Ds, fun(D) -> D:rec2msg(Name) end).

pd([D|Ds], F) ->
    try
        F(D),
        D
    catch
        error:_ ->
            pd(Ds, F)
    end;

pd([], _) ->
    ?ERROR(no_dictionary).

%% send_request/4

send_request(TPid, #diameter_packet{bin = Bin} = Pkt, Req, Timeout)
  when node() == node(TPid) ->
    %% Store the outgoing request before sending to avoid a race with
    %% reply reception.
    TRef = store_request(TPid, Bin, Req, Timeout),
    send(TPid, Pkt),
    TRef;

%% Send using a remote transport: spawn a process on the remote node
%% to relay the answer.
send_request(TPid, #diameter_packet{} = Pkt, Req, Timeout) ->
    TRef = erlang:start_timer(Timeout, self(), timeout),
    T = {TPid, Pkt, Req, Timeout, TRef},
    spawn(node(TPid), ?MODULE, send, [T]),
    TRef.

%% send/1

send({TPid, Pkt, #request{handler = Pid} = Req, Timeout, TRef}) ->
    Ref = send_request(TPid, Pkt, Req#request{handler = self()}, Timeout),
    Pid ! reref(receive T -> T end, Ref, TRef).

reref({T, Ref, R}, Ref, TRef) ->
    {T, TRef, R};
reref(T, _, _) ->
    T.

%% send/2

send(Pid, Pkt) ->
    Pid ! {send, Pkt}.

%% retransmit/4

retransmit({TPid, Caps, #diameter_app{alias = Alias} = App} = T,
           #request{app = Alias, packet = Pkt}
           = Req,
           SvcName,
           Timeout) ->
    have_request(Pkt, TPid)      %% Don't failover to a peer we've
        andalso ?THROW(timeout), %% already sent to.

    resend_req(cb(App, prepare_retransmit, [Pkt, SvcName, {TPid, Caps}]),
               T,
               Req,
               Timeout,
               []).

resend_req({send, P}, T, #request{packet = Pkt} = Req, Timeout, Fs) ->
    retransmit(make_request_packet(P, Pkt), T, Req, Timeout, Fs);

resend_req({discard, Reason}, _, _, _, _) ->
    ?THROW(Reason);

resend_req(discard, _, _, _, _) ->
    ?THROW(discarded);

resend_req({eval_packet, RC, F}, T, Req, Timeout, Fs) ->
    resend_req(RC, T, Req, Timeout, [F|Fs]);
    
resend_req(T, {_, _, App}, _, _, _) ->
    ?ERROR({invalid_return, prepare_retransmit, App, T}).

%% retransmit/6

retransmit(Pkt, {TPid, Caps, _}, #request{dictionary = D} = Req0, Tmo, Fs) ->
    EPkt = encode(D, Pkt, Fs),

    Req = Req0#request{transport = TPid,
                       packet = Pkt,
                       caps = Caps},

    ?LOG(retransmission, Req),
    TRef = send_request(TPid, EPkt, Req, Tmo),
    {TRef, Req}.

%% store_request/4

store_request(TPid, Bin, Req, Timeout) ->
    Seqs = diameter_codec:sequence_numbers(Bin),
    TRef = erlang:start_timer(Timeout, self(), timeout),
    ets:insert(?REQUEST_TABLE, {Seqs, Req, TRef}),
    ets:member(?REQUEST_TABLE, TPid)
        orelse (self() ! {failover, TRef}),  %% possibly missed failover
    TRef.

%% lookup_request/2

lookup_request(Msg, TPid)
  when is_pid(TPid) ->
    lookup(Msg, TPid, '_');

lookup_request(Msg, TRef)
  when is_reference(TRef) ->
    lookup(Msg, '_', TRef).

lookup(Msg, TPid, TRef) ->
    Seqs = diameter_codec:sequence_numbers(Msg),
    Spec = [{{Seqs, #request{transport = TPid, _ = '_'}, TRef},
             [],
             ['$_']}],
    case ets:select(?REQUEST_TABLE, Spec) of
        [{_, Req, _}] ->
            Req;
        [] ->
            false
    end.

%% erase_request/1

erase_request(Pkt) ->
    ets:delete(?REQUEST_TABLE, diameter_codec:sequence_numbers(Pkt)).

%% match_requests/1

match_requests(TPid) ->
    Pat = {'_', #request{transport = TPid, _ = '_'}, '_'},
    ets:select(?REQUEST_TABLE, [{Pat, [], ['$_']}]).

%% have_request/2

have_request(Pkt, TPid) ->
    Seqs = diameter_codec:sequence_numbers(Pkt),
    Pat = {Seqs, #request{transport = TPid, _ = '_'}, '_'},
    '$end_of_table' /= ets:select(?REQUEST_TABLE, [{Pat, [], ['$_']}], 1).

%% request_peer_up/1

request_peer_up(TPid) ->
    ets:insert(?REQUEST_TABLE, {TPid}).

%% request_peer_down/2

request_peer_down(TPid, S) ->
    ets:delete(?REQUEST_TABLE, TPid),
    lists:foreach(fun(T) -> failover(T,S) end, match_requests(TPid)).
%% Note that a request process can store its request after failover
%% notifications are sent here: store_request/4 sends the notification
%% in that case. Note also that we'll send as many notifications to a
%% given handler as there are peers its sent to. All but one of these
%% will be ignored.

%%% ---------------------------------------------------------------------------
%%% recv_request/3
%%% ---------------------------------------------------------------------------

recv_request(TPid, Pkt, {ConnT, SvcName, Apps}) ->  %% upgrade
    recv_request(TPid, Pkt, {ConnT, SvcName, Apps, ?NOMASK});

recv_request(TPid, Pkt, {ConnT, SvcName, Apps, Mask}) ->
    try ets:lookup(ConnT, TPid) of
        [C] ->
            recv_request(C, TPid, Pkt, SvcName, Apps, Mask);
        [] ->             %% transport has gone down
            ok
    catch
        error: badarg ->  %% service has gone down (and taken table with it)
            ok
    end.

%% recv_request/5

recv_request(#conn{apps = SApps, caps = Caps},
             TPid,
             Pkt,
             SvcName,
             Apps,
             Mask) ->
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,

    #diameter_packet{header = #diameter_header{application_id = Id}}
        = Pkt,

    recv_request(find_recv_app(Id, SApps),
                 {SvcName, OH, OR},
                 TPid,
                 Apps,
                 Mask,
                 Caps,
                 Pkt).

%% find_recv_app/2

%% No one should be sending the relay identifier.
find_recv_app(?APP_ID_RELAY, _) ->
    false;

%% With any other id we either support it locally or as a relay.
find_recv_app(Id, SApps) ->
    keyfind([Id, ?APP_ID_RELAY], 1, SApps).

%% keyfind/3

keyfind([], _, _) ->
    false;
keyfind([Key | Rest], Pos, L) ->
    case lists:keyfind(Key, Pos, L) of
        false ->
            keyfind(Rest, Pos, L);
        T ->
            T
    end.

%% recv_request/7

recv_request({Id, Alias}, T, TPid, Apps, Mask, Caps, Pkt) ->
    #diameter_app{dictionary = Dict}
        = A
        = find_app(Alias, Apps),
    recv_request(T,
                 {TPid, Caps},
                 A,
                 Mask,
                 diameter_codec:decode(Id, Dict, Pkt));
%% Note that the decode is different depending on whether or not Id is
%% ?APP_ID_RELAY.

%%   DIAMETER_APPLICATION_UNSUPPORTED   3007
%%      A request was sent for an application that is not supported.

recv_request(false, T, TPid, _, _, _, Pkt) ->
    As = collect_avps(Pkt),
    protocol_error(3007, T, TPid, Pkt#diameter_packet{avps = As}).

collect_avps(Pkt) ->
    case diameter_codec:collect_avps(Pkt) of
        {_Bs, As} ->
            As;
        As ->
            As
    end.

%% recv_request/5

%% Wrong number of bits somewhere in the message: reply.
%%
%%   DIAMETER_INVALID_AVP_BITS          3009
%%      A request was received that included an AVP whose flag bits are
%%      set to an unrecognized value, or that is inconsistent with the
%%      AVP's definition.
%%
recv_request(T, {TPid, _}, _, _, #diameter_packet{errors = [Bs | _]} = Pkt)
  when is_bitstring(Bs) ->
    protocol_error(3009, T, TPid, Pkt);

%% Either we support this application but don't recognize the command
%% or we're a relay and the command isn't proxiable.
%%
%%   DIAMETER_COMMAND_UNSUPPORTED       3001
%%      The Request contained a Command-Code that the receiver did not
%%      recognize or support.  This MUST be used when a Diameter node
%%      receives an experimental command that it does not understand.
%%
recv_request(T,
             {TPid, _},
             #diameter_app{id = Id},
             _,
             #diameter_packet{header = #diameter_header{is_proxiable = P},
                              msg = M}
             = Pkt)
  when ?APP_ID_RELAY /= Id, undefined == M;
       ?APP_ID_RELAY == Id, not P ->
    protocol_error(3001, T, TPid, Pkt);

%% Error bit was set on a request.
%%
%%   DIAMETER_INVALID_HDR_BITS          3008
%%      A request was received whose bits in the Diameter header were
%%      either set to an invalid combination, or to a value that is
%%      inconsistent with the command code's definition.
%%
recv_request(T,
             {TPid, _},
             _,
             _,
             #diameter_packet{header = #diameter_header{is_error = true}}
             = Pkt) ->
    protocol_error(3008, T, TPid, Pkt);

%% A message in a locally supported application or a proxiable message
%% in the relay application. Don't distinguish between the two since
%% each application has its own callback config. That is, the user can
%% easily distinguish between the two cases.
recv_request(T, TC, App, Mask, Pkt) ->
    request_cb(T, TC, App, Mask, examine(Pkt)).

%% Note that there may still be errors but these aren't protocol
%% (3xxx) errors that lead to an answer-message.

request_cb({SvcName, _OH, _OR} = T, TC, App, Mask, Pkt) ->
    request_cb(cb(App, handle_request, [Pkt, SvcName, TC]),
               App,
               Mask,
               T,
               TC,
               [],
               Pkt).

%% examine/1
%%
%% Look for errors in a decoded message. Length errors result in
%% decode failure in diameter_codec.

examine(#diameter_packet{header = #diameter_header{version
                                                   = ?DIAMETER_VERSION}}
        = Pkt) ->
    Pkt;

%%   DIAMETER_UNSUPPORTED_VERSION       5011
%%      This error is returned when a request was received, whose version
%%      number is unsupported.

examine(#diameter_packet{errors = Es} = Pkt) ->
    Pkt#diameter_packet{errors = [5011 | Es]}.
%% It's odd/unfortunate that this isn't a protocol error.

%% request_cb/7

%% A reply may be an answer-message, constructed either here or by
%% the handle_request callback. The header from the incoming request
%% is passed into the encode so that it can retrieve the relevant
%% command code in this case. It will also then ignore Dict and use
%% the base encoder.
request_cb({reply, Ans},
           #diameter_app{dictionary = Dict},
           _,
           _,
           {TPid, _},
           Fs,
           Pkt) ->
    reply(Ans, Dict, TPid, Fs, Pkt);

%% An 3xxx result code, for which the E-bit is set in the header.
request_cb({protocol_error, RC}, _, _, T, {TPid, _}, Fs, Pkt)
  when 3000 =< RC, RC < 4000 ->
    protocol_error(RC, T, TPid, Fs, Pkt);

%% RFC 3588 says we must reply 3001 to anything unrecognized or
%% unsupported. 'noreply' is undocumented (and inappropriately named)
%% backwards compatibility for this, protocol_error the documented
%% alternative.
request_cb(noreply, _, _, T, {TPid, _}, Fs, Pkt) ->
    protocol_error(3001, T, TPid, Fs, Pkt);

%% Relay a request to another peer. This is equivalent to doing an
%% explicit call/4 with the message in question except that (1) a loop
%% will be detected by examining Route-Record AVP's, (3) a
%% Route-Record AVP will be added to the outgoing request and (3) the
%% End-to-End Identifier will default to that in the
%% #diameter_header{} without the need for an end_to_end_identifier
%% option.
%%
%% relay and proxy are similar in that they require the same handling
%% with respect to Route-Record and End-to-End identifier. The
%% difference is that a proxy advertises specific applications, while
%% a relay advertises the relay application. If a callback doesn't
%% want to distinguish between the cases in the callback return value
%% then 'resend' is a neutral alternative.
%%
request_cb({A, Opts},
           #diameter_app{id = Id}
           = App,
           Mask,
           T,
           TC,
           Fs,
           Pkt)
  when A == relay, Id == ?APP_ID_RELAY;
       A == proxy, Id /= ?APP_ID_RELAY;
       A == resend ->
    resend(Opts, App, Mask, T, TC, Fs, Pkt);

request_cb(discard, _, _, _, _, _, _) ->
    ok;

request_cb({eval_packet, RC, F}, App, Mask, T, TC, Fs, Pkt) ->
    request_cb(RC, App, Mask, T, TC, [F|Fs], Pkt);

request_cb({eval, RC, F}, App, Mask, T, TC, Fs, Pkt) ->
    request_cb(RC, App, Mask, T, TC, Pkt, Fs),
    diameter_lib:eval(F).

%% protocol_error/5

protocol_error(RC, {_, OH, OR}, TPid, Fs, Pkt) ->
    #diameter_packet{avps = Avps} = Pkt,
    ?LOG({error, RC}, Pkt),
    reply(answer_message({OH, OR, RC}, Avps), ?BASE, TPid, Fs, Pkt).

%% protocol_error/4

protocol_error(RC, T, TPid, Pkt) ->
    protocol_error(RC, T, TPid, [], Pkt).

%% resend/7
%%
%% Resend a message as a relay or proxy agent.

resend(Opts,
       #diameter_app{} = App,
       Mask,
       {_SvcName, OH, _OR} = T,
       {_TPid, _Caps} = TC,
       Fs,
       #diameter_packet{avps = Avps} = Pkt) ->
    {Code, _Flags, Vid} = ?BASE:avp_header('Route-Record'),
    resend(is_loop(Code, Vid, OH, Avps), Opts, App, Mask, T, TC, Fs, Pkt).

%%   DIAMETER_LOOP_DETECTED             3005
%%      An agent detected a loop while trying to get the message to the
%%      intended recipient.  The message MAY be sent to an alternate peer,
%%      if one is available, but the peer reporting the error has
%%      identified a configuration problem.

resend(true, _, _, _, T, {TPid, _}, Fs, Pkt) ->  %% Route-Record loop
    protocol_error(3005, T, TPid, Fs, Pkt);

%% 6.1.8.  Relaying and Proxying Requests
%%
%%   A relay or proxy agent MUST append a Route-Record AVP to all requests
%%   forwarded.  The AVP contains the identity of the peer the request was
%%   received from.

resend(false,
       Opts,
       App,
       Mask,
       {SvcName, _, _} = T,
       {TPid, #diameter_caps{origin_host = {_, OH}}},
       Fs,
       #diameter_packet{header = Hdr0,
                        avps = Avps}
       = Pkt) ->
    Route = #diameter_avp{data = {?BASE, 'Route-Record', OH}},
    Seq = diameter_session:sequence(Mask),
    Hdr = Hdr0#diameter_header{hop_by_hop_id = Seq},
    Msg = [Hdr, Route | Avps],
    resend(call(SvcName, App, Msg, Opts), T, TPid, Fs, Pkt).
%% The incoming request is relayed with the addition of a
%% Route-Record. Note the requirement on the return from call/4 below,
%% which places a requirement on the value returned by the
%% handle_answer callback of the application module in question.
%%
%% Note that there's nothing stopping the request from being relayed
%% back to the sender. A pick_peer callback may want to avoid this but
%% a smart peer might recognize the potential loop and choose another
%% route. A less smart one will probably just relay the request back
%% again and force us to detect the loop. A pick_peer that wants to
%% avoid this can specify filter to avoid the possibility.
%% Eg. {neg, {host, OH} where #diameter_caps{origin_host = {OH, _}}.
%%
%% RFC 6.3 says that a relay agent does not modify Origin-Host but
%% says nothing about a proxy. Assume it should behave the same way.

%% resend/4
%%
%% Relay a reply to a relayed request.

%% Answer from the peer: reset the hop by hop identifier and send.
resend(#diameter_packet{bin = B}
       = Pkt,
       _,
       TPid,
       Fs,
       #diameter_packet{header = #diameter_header{hop_by_hop_id = Id},
                        transport_data = TD}) ->
    P = Pkt#diameter_packet{bin = diameter_codec:hop_by_hop_id(Id, B),
                                   transport_data = TD},
    eval_packet(P, Fs),
    send(TPid, P);
%% TODO: counters

%% Or not: DIAMETER_UNABLE_TO_DELIVER.
resend(_, T, TPid, Fs, Pkt) ->
    protocol_error(3002, T, TPid, Fs, Pkt).

%% is_loop/4
%%
%% Is there a Route-Record AVP with our Origin-Host?

is_loop(Code,
        Vid,
        Bin,
        [#diameter_avp{code = Code, vendor_id = Vid, data = Bin} | _]) ->
    true;

is_loop(_, _, _, []) ->
    false;

is_loop(Code, Vid, OH, [_ | Avps])
  when is_binary(OH) ->
    is_loop(Code, Vid, OH, Avps);

is_loop(Code, Vid, OH, Avps) ->
    is_loop(Code, Vid, ?BASE:avp(encode, OH, 'Route-Record'), Avps).

%% reply/5
%%
%% Send a locally originating reply.

%% Skip the setting of Result-Code and Failed-AVP's below.
reply([Msg], Dict, TPid, Fs, Pkt)
  when is_list(Msg);
       is_tuple(Msg) ->
    reply(Msg, Dict, TPid, Fs, Pkt#diameter_packet{errors = []});

%% No errors or a diameter_header/avp list.
reply(Msg, Dict, TPid, Fs, #diameter_packet{errors = Es} = ReqPkt)
  when [] == Es;
       is_record(hd(Msg), diameter_header) ->
    Pkt = diameter_codec:encode(Dict, make_answer_packet(Msg, ReqPkt)),
    eval_packet(Pkt, Fs),
    incr(send, Pkt, Dict, TPid),  %% count result codes in sent answers
    send(TPid, Pkt);

%% Or not: set Result-Code and Failed-AVP AVP's.
reply(Msg, Dict, TPid, Fs, #diameter_packet{errors = [H|_] = Es} = Pkt) ->
    reply(rc(Msg, rc(H), [A || {_,A} <- Es], Dict),
          Dict,
          TPid,
          Fs,
          Pkt#diameter_packet{errors = []}).

eval_packet(Pkt, Fs) ->
    lists:foreach(fun(F) -> diameter_lib:eval([F,Pkt]) end, Fs).
    
%% make_answer_packet/2

%% A reply message clears the R and T flags and retains the P flag.
%% The E flag will be set at encode. 6.2 of 3588 requires the same P
%% flag on an answer as on the request. A #diameter_packet{} returned
%% from a handle_request callback can circumvent this by setting its
%% own header values.
make_answer_packet(#diameter_packet{header = Hdr,
                                    msg = Msg,
                                    transport_data = TD},
                   #diameter_packet{header = ReqHdr}) ->
    Hdr0 = ReqHdr#diameter_header{version = ?DIAMETER_VERSION,
                                  is_request = false,
                                  is_error = undefined,
                                  is_retransmitted = false},
    #diameter_packet{header = fold_record(Hdr0, Hdr),
                     msg = Msg,
                     transport_data = TD};

%% Binaries and header/avp lists are sent as-is.
make_answer_packet(Bin, #diameter_packet{transport_data = TD})
  when is_binary(Bin) ->
    #diameter_packet{bin = Bin,
                     transport_data = TD};
make_answer_packet([#diameter_header{} | _] = Msg,
                   #diameter_packet{transport_data = TD}) ->
    #diameter_packet{msg = Msg,
                     transport_data = TD};

%% Otherwise, preserve transport_data.
make_answer_packet(Msg, #diameter_packet{transport_data = TD} = Pkt) ->
    make_answer_packet(#diameter_packet{msg = Msg, transport_data = TD}, Pkt).

%% rc/1

rc({RC, _}) ->
    RC;
rc(RC) ->
    RC.

%% rc/4

rc(Rec, RC, Failed, Dict)
  when is_integer(RC) ->
    set(Rec,
        lists:append([rc(Rec, {'Result-Code', RC}, Dict),
                      failed_avp(Rec, Failed, Dict)]),
        Dict).

%% Reply as name and tuple list ...
set([_|_] = Ans, Avps, _) ->
    Ans ++ Avps;  %% Values nearer tail take precedence.

%% ... or record.
set(Rec, Avps, Dict) ->
    Dict:'#set-'(Avps, Rec).

%% rc/3
%%
%% Turn the result code into a list if its optional and only set it if
%% the arity is 1 or {0,1}. In other cases (which probably shouldn't
%% exist in practise) we can't know what's appropriate.

rc([MsgName | _], {'Result-Code' = K, RC} = T, Dict) ->
    case Dict:avp_arity(MsgName, 'Result-Code') of
        1     -> [T];
        {0,1} -> [{K, [RC]}];
        _     -> []
    end;

rc(Rec, T, Dict) ->
    rc([Dict:rec2msg(element(1, Rec))], T, Dict).

%% failed_avp/3

failed_avp(_, [] = No, _) ->
    No;

failed_avp(Rec, Failed, Dict) ->
    [fa(Rec, [{'AVP', Failed}], Dict)].

%% Reply as name and tuple list ...
fa([MsgName | Values], FailedAvp, Dict) ->
    R = Dict:msg2rec(MsgName),
    try
        Dict:'#info-'(R, {index, 'Failed-AVP'}),
        {'Failed-AVP', [FailedAvp]}
    catch
        error: _ ->
            Avps = proplists:get_value('AVP', Values, []),
            A = #diameter_avp{name = 'Failed-AVP',
                              value = FailedAvp},
            {'AVP', [A|Avps]}
    end;

%% ... or record.
fa(Rec, FailedAvp, Dict) ->
    try
        {'Failed-AVP', [FailedAvp]}
    catch
        error: _ ->
            Avps = Dict:'get-'('AVP', Rec),
            A = #diameter_avp{name = 'Failed-AVP',
                              value = FailedAvp},
            {'AVP', [A|Avps]}
    end.

%% 3.  Diameter Header
%%
%%       E(rror)     - If set, the message contains a protocol error,
%%                     and the message will not conform to the ABNF
%%                     described for this command.  Messages with the 'E'
%%                     bit set are commonly referred to as error
%%                     messages.  This bit MUST NOT be set in request
%%                     messages.  See Section 7.2.

%% 3.2.  Command Code ABNF specification
%%
%%    e-bit            = ", ERR"
%%                       ; If present, the 'E' bit in the Command
%%                       ; Flags is set, indicating that the answer
%%                       ; message contains a Result-Code AVP in
%%                       ; the "protocol error" class.

%% 7.1.3.  Protocol Errors
%%
%%    Errors that fall within the Protocol Error category SHOULD be treated
%%    on a per-hop basis, and Diameter proxies MAY attempt to correct the
%%    error, if it is possible.  Note that these and only these errors MUST
%%    only be used in answer messages whose 'E' bit is set.

%% Thus, only construct answers to protocol errors. Other errors
%% require an message-specific answer and must be handled by the
%% application.

%% 6.2.  Diameter Answer Processing
%%
%%    When a request is locally processed, the following procedures MUST be
%%    applied to create the associated answer, in addition to any
%%    additional procedures that MAY be discussed in the Diameter
%%    application defining the command:
%%
%%    -  The same Hop-by-Hop identifier in the request is used in the
%%       answer.
%%
%%    -  The local host's identity is encoded in the Origin-Host AVP.
%%
%%    -  The Destination-Host and Destination-Realm AVPs MUST NOT be
%%       present in the answer message.
%%
%%    -  The Result-Code AVP is added with its value indicating success or
%%       failure.
%%
%%    -  If the Session-Id is present in the request, it MUST be included
%%       in the answer.
%%
%%    -  Any Proxy-Info AVPs in the request MUST be added to the answer
%%       message, in the same order they were present in the request.
%%
%%    -  The 'P' bit is set to the same value as the one in the request.
%%
%%    -  The same End-to-End identifier in the request is used in the
%%       answer.
%%
%%    Note that the error messages (see Section 7.3) are also subjected to
%%    the above processing rules.

%% 7.3.  Error-Message AVP
%%
%%    The Error-Message AVP (AVP Code 281) is of type UTF8String.  It MAY
%%    accompany a Result-Code AVP as a human readable error message.  The
%%    Error-Message AVP is not intended to be useful in real-time, and
%%    SHOULD NOT be expected to be parsed by network entities.

%% answer_message/2

answer_message({OH, OR, RC}, Avps) ->
    {Code, _, Vid} = ?BASE:avp_header('Session-Id'),
    ['answer-message', {'Origin-Host', OH},
                       {'Origin-Realm', OR},
                       {'Result-Code', RC}
                       | session_id(Code, Vid, Avps)].

session_id(Code, Vid, Avps)
  when is_list(Avps) ->
    try
        {value, #diameter_avp{data = D}} = find_avp(Code, Vid, Avps),
        [{'Session-Id', [?BASE:avp(decode, D, 'Session-Id')]}]
    catch
        error: _ ->
            []
    end.

%% find_avp/3

find_avp(Code, Vid, Avps)
  when is_integer(Code), (undefined == Vid orelse is_integer(Vid)) ->
    find(fun(A) -> is_avp(Code, Vid, A) end, Avps).

%% The final argument here could be a list of AVP's, depending on the case,
%% but we're only searching at the top level.
is_avp(Code, Vid, #diameter_avp{code = Code, vendor_id = Vid}) ->
    true;
is_avp(_, _, _) ->
    false.

find(_, []) ->
    false;
find(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            {value, H};
        false ->
            find(Pred, T)
    end.

%% 7.  Error Handling
%%
%%    There are certain Result-Code AVP application errors that require
%%    additional AVPs to be present in the answer.  In these cases, the
%%    Diameter node that sets the Result-Code AVP to indicate the error
%%    MUST add the AVPs.  Examples are:
%%
%%    -  An unrecognized AVP is received with the 'M' bit (Mandatory bit)
%%       set, causes an answer to be sent with the Result-Code AVP set to
%%       DIAMETER_AVP_UNSUPPORTED, and the Failed-AVP AVP containing the
%%       offending AVP.
%%
%%    -  An AVP that is received with an unrecognized value causes an
%%       answer to be returned with the Result-Code AVP set to
%%       DIAMETER_INVALID_AVP_VALUE, with the Failed-AVP AVP containing the
%%       AVP causing the error.
%%
%%    -  A command is received with an AVP that is omitted, yet is
%%       mandatory according to the command's ABNF.  The receiver issues an
%%       answer with the Result-Code set to DIAMETER_MISSING_AVP, and
%%       creates an AVP with the AVP Code and other fields set as expected
%%       in the missing AVP.  The created AVP is then added to the Failed-
%%       AVP AVP.
%%
%%    The Result-Code AVP describes the error that the Diameter node
%%    encountered in its processing.  In case there are multiple errors,
%%    the Diameter node MUST report only the first error it encountered
%%    (detected possibly in some implementation dependent order).  The
%%    specific errors that can be described by this AVP are described in
%%    the following section.

%% 7.5.  Failed-AVP AVP
%%
%%    The Failed-AVP AVP (AVP Code 279) is of type Grouped and provides
%%    debugging information in cases where a request is rejected or not
%%    fully processed due to erroneous information in a specific AVP.  The
%%    value of the Result-Code AVP will provide information on the reason
%%    for the Failed-AVP AVP.
%%
%%    The possible reasons for this AVP are the presence of an improperly
%%    constructed AVP, an unsupported or unrecognized AVP, an invalid AVP
%%    value, the omission of a required AVP, the presence of an explicitly
%%    excluded AVP (see tables in Section 10), or the presence of two or
%%    more occurrences of an AVP which is restricted to 0, 1, or 0-1
%%    occurrences.
%%
%%    A Diameter message MAY contain one Failed-AVP AVP, containing the
%%    entire AVP that could not be processed successfully.  If the failure
%%    reason is omission of a required AVP, an AVP with the missing AVP
%%    code, the missing vendor id, and a zero filled payload of the minimum
%%    required length for the omitted AVP will be added.

%%% ---------------------------------------------------------------------------
%%% # handle_answer/3
%%% ---------------------------------------------------------------------------

%% Process an answer message in call-specific process.

handle_answer(SvcName, _, {error, Req, Reason}) ->
    handle_error(Req, Reason, SvcName);

handle_answer(SvcName,
              AnswerErrors,
              {answer, #request{dictionary = Dict} = Req, Pkt}) ->
    answer(examine(diameter_codec:decode(Dict, Pkt)),
           SvcName,
           AnswerErrors,
           Req).

%% We don't really need to do a full decode if we're a relay and will
%% just resend with a new hop by hop identifier, but might a proxy
%% want to examine the answer?

answer(Pkt, SvcName, AE, #request{transport = TPid,
                                  dictionary = Dict}
                         = Req) ->
    try
        incr(recv, Pkt, Dict, TPid)
    of
        _ -> a(Pkt, SvcName, AE, Req)
    catch
        exit: {invalid_error_bit, _} = E ->
            a(Pkt#diameter_packet{errors = [E]}, SvcName, AE, Req)
    end.

a(#diameter_packet{errors = Es} = Pkt, SvcName, AE, #request{transport = TPid,
                                                             caps = Caps,
                                                             packet = P}
                                                    = Req)
  when [] == Es;
       callback == AE ->
    cb(Req, handle_answer, [Pkt, msg(P), SvcName, {TPid, Caps}]);

a(Pkt, SvcName, report, Req) ->
    x(errors, handle_answer, [SvcName, Req, Pkt]);

a(Pkt, SvcName, discard, Req) ->
    x({errors, handle_answer, [SvcName, Req, Pkt]}).

%% Note that we don't check that the application id in the answer's
%% header is what we expect. (TODO: Does the rfc says anything about
%% this?)

%% incr/4
%%
%% Increment a stats counter for an incoming or outgoing message.

%% TODO: fix
incr(_, #diameter_packet{msg = undefined}, _, _) ->
    ok;

incr(recv = D, #diameter_packet{header = H, errors = [_|_]}, _, TPid) ->
    incr(TPid, {diameter_codec:msg_id(H), D, error});

incr(Dir, Pkt, Dict, TPid) ->
    #diameter_packet{header = #diameter_header{is_error = E}
                            = Hdr,
                     msg = Rec}
        = Pkt,

    RC = int(get_avp_value(Dict, 'Result-Code', Rec)),
    PE = is_protocol_error(RC),

    %% Check that the E bit is set only for 3xxx result codes.
    (not (E orelse PE))
        orelse (E andalso PE)
        orelse x({invalid_error_bit, RC}, answer, [Dir, Pkt]),

    irc(TPid, Hdr, Dir, rc_counter(Dict, Rec, RC)).

irc(_, _, _, undefined) ->
    false;

irc(TPid, Hdr, Dir, Ctr) ->
    incr(TPid, {diameter_codec:msg_id(Hdr), Dir, Ctr}).

%% incr/2

incr(TPid, Counter) ->
    diameter_stats:incr(Counter, TPid, 1).

%% error_counter/2

%% RFC 3588, 7.6:
%%
%%   All Diameter answer messages defined in vendor-specific
%%   applications MUST include either one Result-Code AVP or one
%%   Experimental-Result AVP.
%%
%% Maintain statistics assuming one or the other, not both, which is
%% surely the intent of the RFC.

rc_counter(Dict, Rec, undefined) ->
    er(get_avp_value(Dict, 'Experimental-Result', Rec));
rc_counter(_, _, RC) ->
    {'Result-Code', RC}.

%% Outgoing answers may be in any of the forms messages can be sent
%% in. Incoming messages will be records. We're assuming here that the
%% arity of the result code AVP's is 0 or 1.

er([{_,_,N} = T | _])
  when is_integer(N) ->
    T;
er({_,_,N} = T)
  when is_integer(N) ->
    T;
er(_) ->
    undefined.

%% Extract the first good looking integer. There's no guarantee
%% that what we're looking for has arity 1.
int([N|_])
  when is_integer(N) ->
    N;
int(N)
  when is_integer(N) ->
    N;
int(_) ->
    undefined.

is_protocol_error(RC) ->
    3000 =< RC andalso RC < 4000.

-spec x(any(), atom(), list()) -> no_return().

%% Warn and exit request process on errors in an incoming answer.
x(Reason, F, A) ->
    diameter_lib:warning_report(Reason, {?MODULE, F, A}),
    x(Reason).

x(T) ->
    exit(T).

%%% ---------------------------------------------------------------------------
%%% # failover/[23]
%%% ---------------------------------------------------------------------------

%% Failover as a consequence of request_peer_down/2.
failover({_, #request{handler = Pid} = Req, TRef}, S) ->
    Pid ! {failover, TRef, rt(Req, S)}.

%% Failover as a consequence of store_request/4.
failover(TRef, Seqs, S)
  when is_reference(TRef) ->
    case lookup_request(Seqs, TRef) of
        #request{} = Req ->
            failover({Seqs, Req, TRef}, S);
        false ->
            ok
    end.

%% prepare_request returned a binary ...
rt(#request{packet = #diameter_packet{msg = undefined}}, _) ->
    false;  %% TODO: Not what we should do.

%% ... or not.
rt(#request{packet = #diameter_packet{msg = Msg},
            dictionary = Dict}
   = Req,
   S) ->
    find_transport(get_destination(Dict, Msg), Req, S).

%%% ---------------------------------------------------------------------------
%%% # report_status/5
%%% ---------------------------------------------------------------------------

report_status(Status,
              #peer{ref = Ref,
                    conn = TPid,
                    type = Type,
                    options = Opts},
              #conn{apps = [_|_] = As,
                    caps = Caps},
              #state{service_name = SvcName}
              = S,
              Extra) ->
    share_peer(Status, Caps, As, TPid, S),
    Info = [Status, Ref, {TPid, Caps}, {type(Type), Opts} | Extra],
    send_event(SvcName, list_to_tuple(Info)).

%% send_event/2

send_event(SvcName, Info) ->
    send_event(#diameter_event{service = SvcName,
                               info = Info}).

send_event(#diameter_event{service = SvcName} = E) ->
    lists:foreach(fun({_, Pid}) -> Pid ! E end, subscriptions(SvcName)).

%%% ---------------------------------------------------------------------------
%%% # share_peer/5
%%% ---------------------------------------------------------------------------

share_peer(up, Caps, Aliases, TPid, #state{options = [_, {_, true} | _],
                                           service_name = Svc}) ->
    diameter_peer:notify(Svc, {peer, TPid, Aliases, Caps});

share_peer(_, _, _, _, _) ->
    ok.

%%% ---------------------------------------------------------------------------
%%% # share_peers/2
%%% ---------------------------------------------------------------------------

share_peers(Pid, #state{options = [_, {_, true} | _],
                        local_peers = PDict}) ->
    ?Dict:fold(fun(A,Ps,ok) -> sp(Pid, A, Ps), ok end, ok, PDict);

share_peers(_, _) ->
    ok.

sp(Pid, Alias, Peers) ->
    lists:foreach(fun({P,C}) -> Pid ! {peer, P, [Alias], C} end, Peers).

%%% ---------------------------------------------------------------------------
%%% # remote_peer_up/4
%%% ---------------------------------------------------------------------------

remote_peer_up(Pid, Aliases, Caps, #state{options = [_, _, {_, true} | _],
                                          service = Svc,
                                          shared_peers = PDict}) ->
    #diameter_service{applications = Apps} = Svc,
    Key = #diameter_app.alias,
    As = lists:filter(fun(A) -> lists:keymember(A, Key, Apps) end, Aliases),
    rpu(Pid, Caps, PDict, As);

remote_peer_up(_, _, _, #state{options = [_, _, {_, false} | _]}) ->
    ok.

rpu(_, _, PDict, []) ->
    PDict;
rpu(Pid, Caps, PDict, Aliases) ->
    erlang:monitor(process, Pid),
    T = {Pid, Caps},
    lists:foreach(fun(A) -> ?Dict:append(A, T, PDict) end, Aliases).

%%% ---------------------------------------------------------------------------
%%% # remote_peer_down/2
%%% ---------------------------------------------------------------------------

remote_peer_down(Pid, #state{options = [_, _, {_, true} | _],
                             shared_peers = PDict}) ->
    lists:foreach(fun(A) -> rpd(Pid, A, PDict) end, ?Dict:fetch_keys(PDict)).

rpd(Pid, Alias, PDict) ->
    ?Dict:update(Alias, fun(Ps) -> lists:keydelete(Pid, 1, Ps) end, PDict).

%%% ---------------------------------------------------------------------------
%%% find_transport/[34]
%%%
%%% Output: {TransportPid, #diameter_caps{}, #diameter_app{}}
%%%         | false
%%%         | {error, Reason}
%%% ---------------------------------------------------------------------------

%% Initial call, from an arbitrary process.
find_transport({alias, Alias}, Msg, Opts, #state{service = Svc} = S) ->
    #diameter_service{applications = Apps} = Svc,
    ft(find_send_app(Alias, Apps), Msg, Opts, S);

%% Relay or proxy send.
find_transport(#diameter_app{} = App, Msg, Opts, S) ->
    ft(App, Msg, Opts, S).

ft(#diameter_app{module = Mod, dictionary = Dict} = App, Msg, Opts, S) ->
    #options{filter = Filter,
             extra = Xtra}
        = Opts,
    pick_peer(App#diameter_app{module = Mod ++ Xtra},
              get_destination(Dict, Msg),
              Filter,
              S);
ft(false = No, _, _, _) ->
    No.

%% This can't be used if we're a relay and sending a message
%% in an application not known locally. (TODO)
find_send_app(Alias, Apps) ->
    case lists:keyfind(Alias, #diameter_app.alias, Apps) of
        #diameter_app{id = ?APP_ID_RELAY} ->
            false;
        T ->
            T
    end.

%% Retransmission, in the service process.
find_transport([_,_] = RH,
               Req,
               #state{service = #diameter_service{pid = Pid,
                                                  applications = Apps}}
               = S)
  when self() == Pid ->
    #request{app = Alias,
             filter = Filter,
             module = ModX}
        = Req,
    #diameter_app{}
        = App
        = lists:keyfind(Alias, #diameter_app.alias, Apps),

    pick_peer(App#diameter_app{module = ModX},
              RH,
              Filter,
              S).

%% get_destination/2

get_destination(Dict, Msg) ->
    [str(get_avp_value(Dict, 'Destination-Realm', Msg)),
     str(get_avp_value(Dict, 'Destination-Host', Msg))].

%% This is not entirely correct. The avp could have an arity 1, in
%% which case an empty list is a DiameterIdentity of length 0 rather
%% than the list of no values we treat it as by mapping to undefined.
%% This behaviour is documented.
str([]) ->
    undefined;
str(T) ->
    T.

%% get_avp_value/3
%%
%% Find an AVP in a message of one of three forms:
%%
%% - a message record (as generated from a .dia spec) or
%% - a list of an atom message name followed by 2-tuple, avp name/value pairs.
%% - a list of a #diameter_header{} followed by #diameter_avp{} records,
%%
%% In the first two forms a dictionary module is used at encode to
%% identify the type of the AVP and its arity in the message in
%% question. The third form allows messages to be sent as is, without
%% a dictionary, which is needed in the case of relay agents, for one.

%% Messages will be header/avps list as a relay and the only AVP's we
%% look for are in the common dictionary. This is required since the
%% relay dictionary doesn't inherit the common dictionary (which maybe
%% it should).
get_avp_value(?RELAY, Name, Msg) ->
    get_avp_value(?BASE, Name, Msg);

%% Message sent as a header/avps list, probably a relay case but not
%% necessarily.
get_avp_value(Dict, Name, [#diameter_header{} | Avps]) ->
    try
        {Code, _, VId} = Dict:avp_header(Name),
        [A|_] = lists:dropwhile(fun(#diameter_avp{code = C, vendor_id = V}) ->
                                        C /= Code orelse V /= VId
                                end,
                                Avps),
        avp_decode(Dict, Name, A)
    catch
        error: _ ->
            undefined
    end;

%% Outgoing message as a name/values list.
get_avp_value(_, Name, [_MsgName | Avps]) ->
    case lists:keyfind(Name, 1, Avps) of
        {_, V} ->
            V;
        _ ->
            undefined
    end;

%% Record might be an answer message in the common dictionary.
get_avp_value(Dict, Name, Rec)
  when Dict /= ?BASE, element(1, Rec) == 'diameter_base_answer-message' ->
    get_avp_value(?BASE, Name, Rec);

%% Message is typically a record but not necessarily: diameter:call/4
%% can be passed an arbitrary term.
get_avp_value(Dict, Name, Rec) ->
    try
        Dict:'#get-'(Name, Rec)
    catch
        error:_ ->
            undefined
    end.

avp_decode(Dict, Name, #diameter_avp{value = undefined,
                                     data = Bin}) ->
    Dict:avp(decode, Bin, Name);
avp_decode(_, _, #diameter_avp{value = V}) ->
    V.

%%% ---------------------------------------------------------------------------
%%% # pick_peer(App, [DestRealm, DestHost], Filter, #state{})
%%%
%%% Output: {TransportPid, #diameter_caps{}, App}
%%%         | false
%%%         | {error, Reason}
%%% ---------------------------------------------------------------------------

%% Find transports to a given realm/host.

pick_peer(#diameter_app{alias = Alias}
          = App,
          [_,_] = RH,
          Filter,
          #state{local_peers = L,
                 shared_peers = S,
                 service_name = SvcName,
                 service = #diameter_service{pid = Pid}}) ->
    pick_peer(peers(Alias, RH, Filter, L),
              peers(Alias, RH, Filter, S),
              Pid,
              SvcName,
              App).

%% pick_peer/5

pick_peer([], [], _, _, _) ->
    false;

%% App state is mutable but we're not in the service process: go there.
pick_peer(Local, Remote, Pid, _SvcName, #diameter_app{mutable = true} = App)
  when self() /= Pid ->
    call_service(Pid, {pick_peer, Local, Remote, App});

%% App state isn't mutable or it is and we're in the service process:
%% do the deed.
pick_peer(Local,
          Remote,
          _Pid,
          SvcName,
          #diameter_app{module = ModX,
                        alias = Alias,
                        init_state = S,
                        mutable = M}
          = App) ->
    MFA = {ModX, pick_peer, [Local, Remote, SvcName]},

    try state_cb(App, MFA) of
        {ok, {TPid, #diameter_caps{} = Caps}} when is_pid(TPid) ->
            {TPid, Caps, App};
        {{TPid, #diameter_caps{} = Caps}, ModS} when is_pid(TPid), M ->
            mod_state(Alias, ModS),
            {TPid, Caps, App};
        {false = No, ModS} when M ->
            mod_state(Alias, ModS),
            No;
        {ok, false = No} ->
            No;
        false = No ->
            No;
        {{TPid, #diameter_caps{} = Caps}, S} when is_pid(TPid) ->
            {TPid, Caps, App};     %% Accept returned state in the immutable
        {false = No, S} ->         %% case as long it isn't changed.
            No;
        T ->
            diameter_lib:error_report({invalid, T, App}, MFA)
    catch
        E: Reason ->
            diameter_lib:error_report({failure, {E, Reason, ?STACK}}, MFA)
    end.

%% peers/4

peers(Alias, RH, Filter, Peers) ->
    case ?Dict:find(Alias, Peers) of
        {ok, L} ->
            ps(L, RH, Filter, {[],[]});
        error ->
            []
    end.

%% Place a peer whose Destination-Host/Realm matches those of the
%% request at the front of the result list. Could add some sort of
%% 'sort' option to allow more control.

ps([], _, _, {Ys, Ns}) ->
    lists:reverse(Ys, Ns);
ps([{_TPid, #diameter_caps{} = Caps} = TC | Rest], RH, Filter, Acc) ->
    ps(Rest, RH, Filter, pacc(caps_filter(Caps, RH, Filter),
                              caps_filter(Caps, RH, {all, [host, realm]}),
                              TC,
                              Acc)).

pacc(true, true, Peer, {Ts, Fs}) ->
    {[Peer|Ts], Fs};
pacc(true, false, Peer, {Ts, Fs}) ->
    {Ts, [Peer|Fs]};
pacc(_, _, _, Acc) ->
    Acc.

%% caps_filter/3

caps_filter(C, RH, {neg, F}) ->
    not caps_filter(C, RH, F);

caps_filter(C, RH, {all, L})
  when is_list(L) ->
    lists:all(fun(F) -> caps_filter(C, RH, F) end, L);

caps_filter(C, RH, {any, L})
  when is_list(L) ->
    lists:any(fun(F) -> caps_filter(C, RH, F) end, L);

caps_filter(#diameter_caps{origin_host = {_,OH}}, [_,DH], host) ->
    eq(undefined, DH, OH);

caps_filter(#diameter_caps{origin_realm = {_,OR}}, [DR,_], realm) ->
    eq(undefined, DR, OR);

caps_filter(C, _, Filter) ->
    caps_filter(C, Filter).

%% caps_filter/2

caps_filter(_, none) ->
    true;

caps_filter(#diameter_caps{origin_host = {_,OH}}, {host, H}) ->
    eq(any, H, OH);

caps_filter(#diameter_caps{origin_realm = {_,OR}}, {realm, R}) ->
    eq(any, R, OR);

%% Anything else is expected to be an eval filter. Filter failure is
%% documented as being equivalent to a non-matching filter.

caps_filter(C, T) ->
    try
        {eval, F} = T,
        diameter_lib:eval([F,C])
    catch
        _:_ -> false
    end.

eq(Any, Id, PeerId) ->
    Any == Id orelse try
                         iolist_to_binary(Id) == iolist_to_binary(PeerId)
                     catch
                         _:_ -> false
                     end.
%% OctetString() can be specified as an iolist() so test for string
%% rather then term equality.

%% transports/1

transports(#state{peerT = PeerT}) ->
    ets:select(PeerT, [{#peer{conn = '$1', _ = '_'},
                        [{'is_pid', '$1'}],
                        ['$1']}]).

%%% ---------------------------------------------------------------------------
%%% # service_info/2
%%% ---------------------------------------------------------------------------

%% The config passed to diameter:start_service/2.
-define(CAP_INFO, ['Origin-Host',
                   'Origin-Realm',
                   'Vendor-Id',
                   'Product-Name',
                   'Origin-State-Id',
                   'Host-IP-Address',
                   'Supported-Vendor-Id',
                   'Auth-Application-Id',
                   'Inband-Security-Id',
                   'Acct-Application-Id',
                   'Vendor-Specific-Application-Id',
                   'Firmware-Revision']).

%% The config returned by diameter:service_info(SvcName, all).
-define(ALL_INFO, [capabilities,
                   applications,
                   transport,
                   pending,
                   options]).

%% The rest.
-define(OTHER_INFO, [connections,
                     name,
                     peers,
                     statistics]).

service_info(Item, S)
  when is_atom(Item) ->
    case tagged_info(Item, S) of
        {_, T} -> T;
        undefined = No -> No
    end;

service_info(Items, S) ->
    tagged_info(Items, S).

tagged_info(Item, S)
  when is_atom(Item) ->
    case complete(Item) of
        {value, I} ->
            {I, complete_info(I,S)};
        false ->
            undefined
    end;

tagged_info(TPid, #state{peerT = PT, connT = CT})
  when is_pid(TPid) ->
    try
        [#conn{peer = Pid}] = ets:lookup(CT, TPid),
        [#peer{ref = Ref, type = Type, options = Opts}] = ets:lookup(PT, Pid),
        [{ref, Ref},
         {type, Type},
         {options, Opts}]
    catch
        error:_ ->
            []
    end;

tagged_info(Items, S)
  when is_list(Items) ->
    [T || I <- Items, T <- [tagged_info(I,S)], T /= undefined, T /= []];

tagged_info(_, _) ->
    undefined.

complete_info(Item, #state{service = Svc} = S) ->
    case Item of
        name ->
            S#state.service_name;
        'Origin-Host' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.origin_host;
        'Origin-Realm' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.origin_realm;
        'Vendor-Id' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.vendor_id;
        'Product-Name' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.product_name;
        'Origin-State-Id' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.origin_state_id;
        'Host-IP-Address' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.host_ip_address;
        'Supported-Vendor-Id' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.supported_vendor_id;
        'Auth-Application-Id' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.auth_application_id;
        'Inband-Security-Id'  ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.inband_security_id;
        'Acct-Application-Id' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.acct_application_id;
        'Vendor-Specific-Application-Id' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.vendor_specific_application_id;
        'Firmware-Revision' ->
            (Svc#diameter_service.capabilities)
                #diameter_caps.firmware_revision;
        capabilities -> service_info(?CAP_INFO, S);
        applications -> info_apps(S);
        transport    -> info_transport(S);
        options      -> info_options(S);
        pending      -> info_pending(S);
        keys         -> ?ALL_INFO ++ ?CAP_INFO ++ ?OTHER_INFO;
        all          -> service_info(?ALL_INFO, S);
        statistics   -> info_stats(S);
        connections  -> info_connections(S);
        peers        -> info_peers(S)
    end.

complete(I)
  when I == keys;
       I == all ->
    {value, I};
complete(Pre) ->
    P = atom_to_list(Pre),
    case [I || I <- ?ALL_INFO ++ ?CAP_INFO ++ ?OTHER_INFO,
               lists:prefix(P, atom_to_list(I))]
    of
        [I] -> {value, I};
        _   -> false
    end.

%% info_stats/1

info_stats(#state{peerT = PeerT}) ->
    MatchSpec = [{#peer{ref = '$1', conn = '$2', _ = '_'},
                  [{'is_pid', '$2'}],
                  [['$1', '$2']]}],
    try ets:select(PeerT, MatchSpec) of
        L ->
            diameter_stats:read(lists:append(L))
    catch
        error: badarg -> []  %% service  has gone down
    end.

%% info_transport/1
%%
%% One entry per configured transport. Statistics for each entry are
%% the accumulated values for the ref and associated peer pids.

info_transport(S) ->
    PeerD = peer_dict(S, config_dict(S)),
    RefsD = dict:map(fun(_, Ls) -> [P || L <- Ls, {peer, {P,_}} <- L] end,
                     PeerD),
    Refs = lists:append(dict:fold(fun(R, Ps, A) -> [[R|Ps] | A] end,
                                  [],
                                  RefsD)),
    Stats = diameter_stats:read(Refs),
    dict:fold(fun(R, Ls, A) ->
                      Ps = dict:fetch(R, RefsD),
                      [[{ref, R} | transport(Ls)] ++ [stats([R|Ps], Stats)]
                       | A]
              end,
              [],
              PeerD).

%% Only a config entry for a listening transport: use it.
transport([[{type, listen}, _] = L]) ->
    L ++ [{accept, []}];

%% Only one config or peer entry for a connecting transport: use it.
transport([[{type, connect} | _] = L]) ->
    L;

%% Peer entries: discard config. Note that the peer entries have
%% length at least 3.
transport([[_,_] | L]) ->
    transport(L);

%% Possibly many peer entries for a listening transport. Note that all
%% have the same options by construction, which is not terribly space
%% efficient. (TODO: all entries for the same Ref should share options.)
transport([[{type, accept}, {options, Opts} | _] | _] = Ls) ->
    [{type, listen},
     {options, Opts},
     {accept, [lists:nthtail(2,L) || L <- Ls]}].

peer_dict(#state{peerT = PeerT, connT = ConnT}, Dict0) ->
    try ets:tab2list(PeerT) of
        L ->
            lists:foldl(fun(T,A) -> peer_acc(ConnT, A, T) end, Dict0, L)
    catch
        error: badarg -> Dict0  %% service has gone down
    end.

peer_acc(ConnT, Acc, #peer{pid = Pid,
                           type = Type,
                           ref = Ref,
                           options = Opts,
                           op_state = OS,
                           started = T,
                           conn = TPid}) ->
    WS = wd_state(OS),
    dict:append(Ref,
                [{type, Type},
                 {options, Opts},
                 {watchdog, {Pid, T, WS}}
                 | info_conn(ConnT, TPid, WS /= ?WD_DOWN)],
                Acc).

info_conn(ConnT, TPid, true)
  when is_pid(TPid) ->
    try ets:lookup(ConnT, TPid) of
        T -> info_conn(T)
    catch
        error: badarg -> []  %% service has gone down
    end;
info_conn(_, _, _) ->
    [].

%% The point of extracting the config here is so that 'transport' info
%% has one entry for each transport ref, the peer table only
%% containing entries that have a living watchdog.

config_dict(#state{service_name = SvcName}) ->
    lists:foldl(fun config_acc/2,
                dict:new(),
                diameter_config:lookup(SvcName)).

config_acc({Ref, T, Opts}, Dict)
  when T == listen;
       T == connect ->
    dict:store(Ref, [[{type, T}, {options, Opts}]], Dict);
config_acc(_, Dict) ->
    Dict.

wd_state({_,S}) ->
    S;
wd_state(?STATE_UP) ->
    ?WD_OKAY;
wd_state(?STATE_DOWN) ->
    ?WD_DOWN.

info_conn([#conn{pid = Pid, apps = SApps, caps = Caps, started = T}]) ->
    [{peer, {Pid, T}},
     {apps, SApps},
     {caps, info_caps(Caps)}
     | try [{port, info_port(Pid)}] catch _:_ -> [] end];
info_conn([] = No) ->
    No.

%% Extract information that the processes involved are expected to
%% "publish" in their process dictionaries. Simple but backhanded.
info_port(Pid) ->
    {_, PD} = process_info(Pid, dictionary),
    {_, T} = lists:keyfind({diameter_peer_fsm, start}, 1, PD),
    {TPid, {_Type, TMod, _Cfg}} = T,
    {_, TD} = process_info(TPid, dictionary),
    {_, Data} = lists:keyfind({TMod, info}, 1, TD),
    [{owner, TPid},
     {module, TMod}
     | try TMod:info(Data) catch _:_ -> [] end].

%% Use the fields names from diameter_caps instead of
%% diameter_base_CER to distinguish between the 2-tuple values
%% compared to the single capabilities values. Note also that the
%% returned list is tagged 'caps' rather than 'capabilities' to
%% emphasize the difference.
info_caps(#diameter_caps{} = C) ->
    lists:zip(record_info(fields, diameter_caps), tl(tuple_to_list(C))).

info_apps(#state{service = #diameter_service{applications = Apps}}) ->
    lists:map(fun mk_app/1, Apps).

mk_app(#diameter_app{} = A) ->
    lists:zip(record_info(fields, diameter_app), tl(tuple_to_list(A))).

%% info_pending/1
%%
%% One entry for each outgoing request whose answer is outstanding.

info_pending(#state{} = S) ->
    MatchSpec = [{{'$1',
                   #request{transport = '$2',
                            from = '$3',
                            app = '$4',
                            _ = '_'},
                   '_'},
                  [?ORCOND([{'==', T, '$2'} || T <- transports(S)])],
                  [{{'$1', [{{app, '$4'}},
                            {{transport, '$2'}},
                            {{from, '$3'}}]}}]}],

    try
        ets:select(?REQUEST_TABLE, MatchSpec)
    catch
        error: badarg -> []  %% service has gone down
    end.

%% info_connections/1
%%
%% One entry per transport connection. Statistics for each entry are
%% for the peer pid only.

info_connections(S) ->
    ConnL = conn_list(S),
    Stats = diameter_stats:read([P || L <- ConnL, {peer, {P,_}} <- L]),
    [L ++ [stats([P], Stats)] || L <- ConnL, {peer, {P,_}} <- L].

conn_list(S) ->
    lists:append(dict:fold(fun conn_acc/3, [], peer_dict(S, dict:new()))).

conn_acc(Ref, Peers, Acc) ->
    [[[{ref, Ref} | L] || L <- Peers, lists:keymember(peer, 1, L)]
     | Acc].

stats(Refs, Stats) ->
    {statistics, dict:to_list(lists:foldl(fun(R,D) ->
                                                  stats_acc(R, D, Stats)
                                          end,
                                          dict:new(),
                                          Refs))}.

stats_acc(Ref, Dict, Stats) ->
    lists:foldl(fun({C,N}, D) -> dict:update_counter(C, N, D) end,
                Dict,
                proplists:get_value(Ref, Stats, [])).

%% info_peers/1
%%
%% One entry per peer Origin-Host. Statistics for each entry are
%% accumulated values for all peer pids.

info_peers(S) ->
    {PeerD, RefD} = lists:foldl(fun peer_acc/2,
                                {dict:new(), dict:new()},
                                conn_list(S)),
    Refs = lists:append(dict:fold(fun(_, Rs, A) -> [Rs|A] end,
                                  [],
                                  RefD)),
    Stats = diameter_stats:read(Refs),
    dict:fold(fun(OH, Cs, A) ->
                      Rs = dict:fetch(OH, RefD),
                      [{OH, [{connections, Cs}, stats(Rs, Stats)]} | A]
              end,
              [],
              PeerD).

peer_acc(Peer, {PeerD, RefD}) ->
    [{TPid, _}, [{origin_host, {_, OH}} | _]]
        = [proplists:get_value(K, Peer) || K <- [peer, caps]],
    {dict:append(OH, Peer, PeerD), dict:append(OH, TPid, RefD)}.

%% info_options/1

info_options(S) ->
    S#state.options.
