%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

-compile({no_auto_import, [now/0]}).
-import(diameter_lib, [now/0]).

%% towards diameter_service_sup
-export([start_link/1]).

%% towards diameter
-export([subscribe/1,
         unsubscribe/1,
         services/0,
         info/2]).

%% towards diameter_config
-export([start/1,
         stop/1,
         start_transport/2,
         stop_transport/2]).

%% towards diameter_peer
-export([notify/2]).

%% towards diameter_traffic
-export([find_incoming_app/4,
         pick_peer/3]).

%% test/debug
-export([services/1,
         subscriptions/1,
         subscriptions/0,
         call_module/3,
         whois/1,
         state/1,
         uptime/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% RFC 3539 watchdog states.
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
-define(RESTART_TC,      1000).  %% if restart was this recent

%% Used to be able to swap this with anything else dict-like but now
%% rely on the fact that a service's #state{} record does not change
%% in storing in it ?STATE table and not always going through the
%% service process. In particular, rely on the fact that operations on
%% a ?Dict don't change the handle to it.
-define(Dict, diameter_dict).

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
   :: T | '_' | '$1' | '$2'.

%% State of service gen_server. Note that the state term itself
%% doesn't change, which is relevant for the stateless application
%% callbacks since the state is retrieved from ?STATE_TABLE from
%% outside the service process. The pid in the service record is used
%% to determine whether or not we need to call the process for a
%% pick_peer callback in the statefull case.
-record(state,
        {id = now(),
         service_name :: diameter:service_name(), %% key in ?STATE_TABLE
         service :: #diameter_service{},
         watchdogT = ets_new(watchdogs) %% #watchdog{} at start
                  :: ets:tid(),
         peerT = ets_new(peers)         %% #peer{pid = TPid} at okay/reopen
              :: ets:tid(),
         shared_peers = ?Dict:new()          %% Alias -> [{TPid, Caps}, ...]
                     :: ets:tid(),
         local_peers  = ?Dict:new()          %% Alias -> [{TPid, Caps}, ...]
                     :: ets:tid(),
         monitor = false :: false | pid(),   %% process to die with
         options
         :: [{sequence, diameter:sequence()}  %% sequence mask
             | {share_peers, diameter:remotes()}       %% broadcast to
             | {use_shared_peers, diameter:remotes()}  %% use from
             | {restrict_connections, diameter:restriction()}
             | {string_decode, boolean()}
             | {incoming_maxlen, diameter:message_length()}]}).
%% shared_peers reflects the peers broadcast from remote nodes.

%% Record representing an RFC 3539 watchdog process implemented by
%% diameter_watchdog.
-record(watchdog,
        {pid  :: match(pid()),
         type :: match(connect | accept),
         ref  :: match(reference()),  %% key into diameter_config
         options :: match([diameter:transport_opt()]),%% from start_transport
         state = ?WD_INITIAL :: match(wd_state()),
         started = now(),      %% at process start
         peer = false :: match(boolean() | pid())}).
                      %% true at accepted, pid() at okay/reopen

%% Record representing an Peer State Machine processes implemented by
%% diameter_peer_fsm.
-record(peer,
        {pid   :: pid(),
         apps  :: [{0..16#FFFFFFFF, diameter:app_alias()}], %% {Id, Alias}
         caps  :: #diameter_caps{},
         started = now(),  %% at process start
         watchdog :: pid()}). %% key into watchdogT

%% ---------------------------------------------------------------------------
%% # start/1
%% ---------------------------------------------------------------------------

start(SvcName) ->
    diameter_service_sup:start_child(SvcName).

start_link(SvcName) ->
    Options = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(?MODULE, [SvcName], Options).
%% Put the arbitrary term SvcName in a list in case we ever want to
%% send more than this and need to distinguish old from new.

%% ---------------------------------------------------------------------------
%% # stop/1
%% ---------------------------------------------------------------------------

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

%% ---------------------------------------------------------------------------
%% # start_transport/3
%% ---------------------------------------------------------------------------

start_transport(SvcName, {_Ref, _Type, _Opts} = T) ->
    call_service_by_name(SvcName, {start, T}).

%% ---------------------------------------------------------------------------
%% # stop_transport/2
%% ---------------------------------------------------------------------------

stop_transport(_, []) ->
    ok;
stop_transport(SvcName, [_|_] = Refs) ->
    call_service_by_name(SvcName, {stop, Refs}).

%% ---------------------------------------------------------------------------
%% # info/2
%% ---------------------------------------------------------------------------

info(SvcName, Item) ->
    case lookup_state(SvcName) of
        [#state{} = S] ->
            service_info(Item, S);
        [] ->
            undefined
    end.

%% lookup_state/1

lookup_state(SvcName) ->
    ets:lookup(?STATE_TABLE, SvcName).

%% ---------------------------------------------------------------------------
%% # subscribe/1
%% # unsubscribe/1
%% ---------------------------------------------------------------------------

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

%% ---------------------------------------------------------------------------
%% # services/1
%% ---------------------------------------------------------------------------

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

%% ---------------------------------------------------------------------------
%% # pick_peer/3
%% ---------------------------------------------------------------------------

-spec pick_peer(SvcName, AppOrAlias, Opts)
   -> {{TPid, Caps, App}, Mask, SvcOpts}
    | false  %% no selection
    | {error, no_service}
 when SvcName :: diameter:service_name(),
      AppOrAlias :: #diameter_app{}
                  | {alias, diameter:app_alias()},
      Opts :: {fun((Dict :: module()) -> [term()]),
               diameter:peer_filter(),
               Xtra :: list()},
      TPid :: pid(),
      Caps :: #diameter_caps{},
      App  :: #diameter_app{},
      Mask :: diameter:sequence(),
      SvcOpts :: [diameter:service_opt()].
%% Extract Mask in the returned tuple so that diameter_traffic doesn't
%% need to know about the ordering of SvcOpts used here.

pick_peer(SvcName, App, Opts) ->
    pick(lookup_state(SvcName), App, Opts).

pick([], _, _) ->
    {error, no_service};

pick([S], App, Opts) ->
    pick(S, App, Opts);

pick(#state{service = #diameter_service{applications = Apps}}
     = S,
     {alias, Alias},
     Opts) ->  %% initial call from diameter:call/4
    pick(S, find_outgoing_app(Alias, Apps), Opts);

pick(_, false = No, _) ->
    No;

pick(#state{options = [{_, Mask} | SvcOpts]}
     = S,
     #diameter_app{module = ModX, dictionary = Dict}
     = App0,
     {DestF, Filter, Xtra}) ->
    App = App0#diameter_app{module = ModX ++ Xtra},
    [_,_] = RealmAndHost = diameter_lib:eval([DestF, Dict]),
    case pick_peer(App, RealmAndHost, Filter, S) of
        {TPid, Caps} ->
            {{TPid, Caps, App}, Mask, SvcOpts};
        false = No ->
            No
    end.

%% ---------------------------------------------------------------------------
%% # find_incoming_app/4
%% ---------------------------------------------------------------------------

-spec find_incoming_app(PeerT, TPid, Id, Apps)
   -> {#diameter_app{}, #diameter_caps{}}  %% connection and suitable app
    | #diameter_caps{}                     %% connection but no suitable app
    | false                                %% no connection
 when PeerT :: ets:tid(),
      TPid  :: pid(),
      Id    :: non_neg_integer(),
      Apps  :: [#diameter_app{}].

find_incoming_app(PeerT, TPid, Id, Apps) ->
    try ets:lookup(PeerT, TPid) of
        [#peer{} = P] ->
            find_incoming_app(P, Id, Apps);
        [] ->             %% transport has gone down
            false
    catch
        error: badarg ->  %% service has gone down (and taken table with it)
            false
    end.

%% ---------------------------------------------------------------------------
%% # notify/2
%% ---------------------------------------------------------------------------

notify(SvcName, Msg) ->
    Pid = whois(SvcName),
    is_pid(Pid) andalso (Pid ! Msg).

%% ===========================================================================
%% ===========================================================================

state(Svc) ->
    call_service(Svc, state).

uptime(Svc) ->
    call_service(Svc, uptime).

%% call_module/3

call_module(Service, AppMod, Request) ->
    call_service(Service, {call_module, AppMod, Request}).

%% ---------------------------------------------------------------------------
%% # init/1
%% ---------------------------------------------------------------------------

init([SvcName]) ->
    process_flag(trap_exit, true),  %% ensure terminate(shutdown, _)
    i(SvcName, diameter_reg:add_new({?MODULE, service, SvcName})).

i(SvcName, true) ->
    {ok, i(SvcName)};
i(_, false) ->
    {stop, {shutdown, already_started}}.

%% ---------------------------------------------------------------------------
%% # handle_call/3
%% ---------------------------------------------------------------------------

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

handle_call(stop, _From, S) ->
    shutdown(service, S),
    {stop, normal, ok, S};
%% The server currently isn't guaranteed to be dead when the caller
%% gets the reply. We deal with this in the call to the server,
%% stating a monitor that waits for DOWN before returning.

handle_call(Req, From, S) ->
    unexpected(handle_call, [Req, From], S),
    {reply, nok, S}.

%% ---------------------------------------------------------------------------
%% # handle_cast/2
%% ---------------------------------------------------------------------------

handle_cast(Req, S) ->
    unexpected(handle_cast, [Req], S),
    {noreply, S}.

%% ---------------------------------------------------------------------------
%% # handle_info/2
%% ---------------------------------------------------------------------------

handle_info(T, #state{} = S) ->
    case transition(T,S) of
        ok ->
            {noreply, S};
        {stop, Reason} ->
            {stop, {shutdown, Reason}, S}
    end.

%% transition/2

%% Peer process is telling us to start a new accept process.
transition({accepted, Pid, TPid}, S) ->
    accepted(Pid, TPid, S),
    ok;

%% Connecting transport is being restarted by watchdog.
transition({reconnect, Pid}, S) ->
    reconnect(Pid, S),
    ok;

%% Watchdog is sending notification of transport death.
transition({close, Pid, Reason}, #state{service_name = SvcName,
                                        watchdogT = WatchdogT}) ->
    #watchdog{state = WS,
              ref = Ref,
              type = Type,
              options = Opts}
        = fetch(WatchdogT, Pid),
    WS /= ?WD_OKAY
        andalso
        send_event(SvcName, {closed, Ref, Reason, {type(Type), Opts}}),
    ok;

%% Watchdog is sending notification of a state transition.
transition({watchdog, Pid, {[TPid | Data], From, To}},
           #state{service_name = SvcName,
                  watchdogT = WatchdogT}
           = S) ->
    #watchdog{ref = Ref, type = T, options = Opts}
        = Wd
        = fetch(WatchdogT, Pid),
    watchdog(TPid, Data, From, To, Wd, S),
    send_event(SvcName, {watchdog, Ref, TPid, {From, To}, {T, Opts}}),
    ok;
%% Death of a watchdog process (#watchdog.pid) results in the removal of
%% it's peer and any associated conn record when 'DOWN' is received.
%% Death of a peer process process (#peer.pid, #watchdog.peer) results in
%% ?WD_DOWN.

%% Monitor process has died. Just die with a reason that tells
%% diameter_config about the happening. If a cleaner shutdown is
%% required then someone should stop us.
transition({'DOWN', MRef, process, _, Reason}, #state{monitor = MRef}) ->
    {stop, {monitor, Reason}};

%% Local watchdog process has died.
transition({'DOWN', _, process, Pid, _Reason}, S)
  when node(Pid) == node() ->
    watchdog_down(Pid, S),
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

transition(Req, S) ->
    unexpected(handle_info, [Req], S),
    ok.

%% ---------------------------------------------------------------------------
%% # terminate/2
%% ---------------------------------------------------------------------------

terminate(Reason, #state{service_name = Name, peerT = PeerT} = S) ->
    send_event(Name, stop),
    ets:delete(?STATE_TABLE, Name),

    %% Communicate pending loss of any peers that connection_down/3
    %% won't. This is needed when stopping a service since we don't
    %% wait for watchdog state changes to take care of if. That this
    %% takes place after deleting the state entry ensures that the
    %% resulting failover by request processes accomplishes nothing.
    ets:foldl(fun(#peer{pid = TPid}, _) ->
                      diameter_traffic:peer_down(TPid)
              end,
              ok,
              PeerT),

    shutdown == Reason  %% application shutdown
        andalso shutdown(application, S).

%% ---------------------------------------------------------------------------
%% # code_change/3
%% ---------------------------------------------------------------------------

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

cb(#diameter_app{module = [_|_] = M}, F, A) ->
    eval(M, F, A).

eval([M|X], F, A) ->
    apply(M, F, A ++ X).

%% Callback with state.

state_cb(#diameter_app{module = ModX, mutable = false, init_state = S},
         pick_peer = F,
         A) ->
    eval(ModX, F, A ++ [S]);

state_cb(#diameter_app{module = ModX, alias = Alias}, F, A) ->
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

%% ---------------------------------------------------------------------------
%% # shutdown/2
%% ---------------------------------------------------------------------------

%% remove_transport
shutdown(Refs, #state{watchdogT = WatchdogT})
  when is_list(Refs) ->
    ets:foldl(fun(P,ok) -> st(P, Refs), ok end, ok, WatchdogT);

%% application/service shutdown
shutdown(Reason, #state{watchdogT = WatchdogT})
  when Reason == application;
       Reason == service ->
    diameter_lib:wait(ets:foldl(fun(P,A) -> st(P, Reason, A) end,
                                [],
                                WatchdogT)).

%% st/2

st(#watchdog{ref = Ref, pid = Pid}, Refs) ->
    lists:member(Ref, Refs)
        andalso (Pid ! {shutdown, self(), transport}).  %% 'DOWN' cleans up

%% st/3

st(#watchdog{pid = Pid}, Reason, Acc) ->
    MRef = monitor(process, Pid),
    Pid ! {shutdown, self(), Reason},
    [MRef | Acc].

%% ---------------------------------------------------------------------------
%% # call_service/2
%% ---------------------------------------------------------------------------

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

%% ---------------------------------------------------------------------------
%% # i/1
%% ---------------------------------------------------------------------------

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
    send_event(SvcName, start),
    lists:foreach(fun(T) -> start_fsm(T,S) end, CL),

    init_shared(S),
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
                                                ?RESTRICT)},
     {spawn_opt, proplists:get_value(spawn_opt, Opts, [])},
     {string_decode, proplists:get_value(string_decode, Opts, true)},
     {incoming_maxlen, proplists:get_value(incoming_maxlen, Opts, 16#FFFFFF)}].
%% The order of options is significant since we match against the list.

mref(false = No) ->
    No;
mref(P) ->
    erlang:monitor(process, P).

init_shared(#state{options = [_, _, {_,T} | _],
                   service_name = Svc}) ->
    notify(T, Svc, {service, self()}).

init_mod(#diameter_app{alias = Alias,
                       init_state = S}) ->
    mod_state(Alias, S).

start_fsm({Ref, Type, Opts}, S) ->
    start(Ref, {Type, Opts}, S).

get_value(Key, Vs) ->
    {_, V} = lists:keyfind(Key, 1, Vs),
    V.

notify(Share, SvcName, T) ->
    Nodes = remotes(Share),
    [] /= Nodes andalso diameter_peer:notify(Nodes, SvcName, T).
%% Test for the empty list for upgrade reasons: there's no
%% diameter_peer:notify/3 in old code.

remotes(false) ->
    [];

remotes(true) ->
    nodes();

remotes(Nodes)
  when is_atom(hd(Nodes));
       Nodes == [] ->
    Nodes;

remotes(F) ->
    try diameter_lib:eval(F) of
        L when is_list(L) ->
            L;
        T ->
            ?LOG(invalid_return, {F,T}),
            error_report(invalid_return, share_peers, F),
            []
    catch
        E:R ->
            ?LOG(failure, {E, R, F, diameter_lib:get_stacktrace()}),
            error_report(failure, share_peers, F),
            []
    end.

%% error_report/3

error_report(T, What, F) ->
    Reason = io_lib:format("~s from ~p callback", [reason(T), What]),
    diameter_lib:error_report(Reason, diameter_lib:eval_name(F)).

reason(invalid_return) ->
    "invalid return";
reason(failure) ->
    "failure".

%% ---------------------------------------------------------------------------
%% # start/3
%% ---------------------------------------------------------------------------

%% If the initial start/3 at service/transport start succeeds then
%% subsequent calls to start/4 on the same service will also succeed
%% since they involve the same call to merge_service/2. We merge here
%% rather than earlier since the service may not yet be configured
%% when the transport is configured.

start(Ref, {T, Opts}, S)
  when T == connect;
       T == listen ->
    N = proplists:get_value(pool_size, Opts, 1),
    try
        {ok, start(Ref, type(T), Opts, N, S)}
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

start(Ref, Type, Opts, State) ->
    start(Ref, Type, Opts, 1, State).

%% start/5

start(Ref, Type, Opts, N, #state{watchdogT = WatchdogT,
                                 peerT = PeerT,
                                 options = SvcOpts,
                                 service_name = SvcName,
                                 service = Svc0})
  when Type == connect;
       Type == accept ->
    #diameter_service{applications = Apps}
        = Svc1
        = merge_service(Opts, Svc0),
    Svc = binary_caps(Svc1, proplists:get_value(string_decode, SvcOpts, true)),
    RecvData = diameter_traffic:make_recvdata([SvcName,
                                               PeerT,
                                               Apps,
                                               SvcOpts]),
    T = {{spawn_opts([Opts, SvcOpts]), RecvData}, Opts, SvcOpts, Svc},
    Rec = #watchdog{type = Type,
                    ref = Ref,
                    options = Opts},
    diameter_lib:fold_n(fun(_,A) ->
                                [wd(Type, Ref, T, WatchdogT, Rec) | A]
                        end,
                        [],
                        N).

binary_caps(Svc, true) ->
    Svc;
binary_caps(#diameter_service{capabilities = Caps} = Svc, false) ->
    Svc#diameter_service{capabilities = diameter_capx:binary_caps(Caps)}.

wd(Type, Ref, T, WatchdogT, Rec) ->
    Pid = start_watchdog(Type, Ref, T),
    insert(WatchdogT, Rec#watchdog{pid = Pid}),
    Pid.

%% Note that the service record passed into the watchdog is the merged
%% record so that each watchdog may get a different record. This
%% record is what is passed back into application callbacks.

spawn_opts(Optss) ->
    SpawnOpts = get_value(spawn_opt, Optss, []),
    [T || T <- SpawnOpts,
          T /= link,
          T /= monitor].

start_watchdog(Type, Ref, T) ->
    {_MRef, Pid} = diameter_watchdog:start({Type, Ref}, T),
    Pid.

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
%% locally implemented Diameter node as identified by Origin-Host: a
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

%% ---------------------------------------------------------------------------
%% # accepted/3
%% ---------------------------------------------------------------------------

accepted(Pid, _TPid, #state{watchdogT = WatchdogT} = S) ->
    #watchdog{ref = Ref, type = accept = T, peer = false, options = Opts}
        = Wd
        = fetch(WatchdogT, Pid),
    insert(WatchdogT, Wd#watchdog{peer = true}),%% mark replacement as started
    start(Ref, T, Opts, S).                     %% start new watchdog

fetch(Tid, Key) ->
    [T] = ets:lookup(Tid, Key),
    T.

%% ---------------------------------------------------------------------------
%% # watchdog/6
%%
%% React to a watchdog state transition.
%% ---------------------------------------------------------------------------

%% Watchdog has a new open connection.
watchdog(TPid, [T], _, ?WD_OKAY, Wd, State) ->
    connection_up({TPid, T}, Wd, State);

%% Watchdog has a new connection that will be opened after DW[RA]
%% exchange.
watchdog(TPid, [T], _, ?WD_REOPEN, Wd, State) ->
    reopen({TPid, T}, Wd, State);

%% Watchdog has recovered a suspect connection.
watchdog(TPid, [], ?WD_SUSPECT, ?WD_OKAY, Wd, State) ->
    #watchdog{peer = TPid} = Wd,  %% assert
    connection_up(Wd, State);

%% Watchdog has an unresponsive connection.
watchdog(TPid, [], ?WD_OKAY, ?WD_SUSPECT = To, Wd, State) ->
    #watchdog{peer = TPid} = Wd,  %% assert
    watchdog_down(Wd, To, State);

%% Watchdog has lost its connection.
watchdog(TPid, [], _, ?WD_DOWN = To, Wd, #state{peerT = PeerT} = S) ->
    close(Wd),
    watchdog_down(Wd, To, S),
    ets:delete(PeerT, TPid);

watchdog(_, [], _, _, _, _) ->
    ok.

watchdog_down(Wd, To, #state{watchdogT = WatchdogT} = S) ->
    insert(WatchdogT, Wd#watchdog{state = To}),
    connection_down(Wd, To, S).

%% ---------------------------------------------------------------------------
%% # connection_up/3
%% ---------------------------------------------------------------------------

%% Watchdog process has reached state OKAY.

connection_up({TPid, {Caps, SupportedApps, Pkt}},
              #watchdog{pid = Pid}
              = Wd,
              #state{peerT = PeerT}
              = S) ->
    Pr = #peer{pid = TPid,
               apps = SupportedApps,
               caps = Caps,
               watchdog = Pid},
    insert(PeerT, Pr),
    connection_up([Pkt], Wd#watchdog{peer = TPid}, Pr, S).

%% ---------------------------------------------------------------------------
%% # reopen/3
%% ---------------------------------------------------------------------------

reopen({TPid, {Caps, SupportedApps, _Pkt}},
       #watchdog{pid = Pid}
       = Wd,
       #state{watchdogT = WatchdogT,
              peerT = PeerT}) ->
    insert(PeerT, #peer{pid = TPid,
                        apps = SupportedApps,
                        caps = Caps,
                        watchdog = Pid}),
    insert(WatchdogT, Wd#watchdog{state = ?WD_REOPEN,
                                  peer = TPid}).

%% ---------------------------------------------------------------------------
%% # connection_up/2
%% ---------------------------------------------------------------------------

%% Watchdog has recovered as suspect connection. Note that there has
%% been no new capabilties exchange in this case.

connection_up(#watchdog{peer = TPid} = Wd, #state{peerT = PeerT} = S) ->
    connection_up([], Wd, fetch(PeerT, TPid), S).

%% connection_up/4

connection_up(Extra,
              #watchdog{peer = TPid}
              = Wd,
              #peer{apps = SApps, caps = Caps}
              = Pr,
              #state{watchdogT = WatchdogT,
                     local_peers = LDict,
                     service_name = SvcName,
                     service = #diameter_service{applications = Apps}}
              = S) ->
    insert(WatchdogT, Wd#watchdog{state = ?WD_OKAY}),
    diameter_traffic:peer_up(TPid),
    insert_local_peer(SApps, {{TPid, Caps}, {SvcName, Apps}}, LDict),
    report_status(up, Wd, Pr, S, Extra).

insert_local_peer(SApps, T, LDict) ->
    lists:foldl(fun(A,D) -> ilp(A, T, D) end, LDict, SApps).

ilp({Id, Alias}, {TC, SA}, LDict) ->
    init_conn(Id, Alias, TC, SA),
    ?Dict:append(Alias, TC, LDict).

init_conn(Id, Alias, {TPid, _} = TC, {SvcName, Apps}) ->
    #diameter_app{id = Id}  %% assert
        = App
        = find_app(Alias, Apps),

    peer_cb(App, peer_up, [SvcName, TC])
        orelse exit(TPid, kill).  %% fake transport failure

%% ---------------------------------------------------------------------------
%% # find_incoming_app/3
%% ---------------------------------------------------------------------------

%% No one should be sending the relay identifier.
find_incoming_app(#peer{caps = Caps}, ?APP_ID_RELAY, _) ->
    Caps;

find_incoming_app(Peer, Id, Apps)
  when is_integer(Id) ->
    find_incoming_app(Peer, [Id, ?APP_ID_RELAY], Apps);

%% Note that the apps represented in SApps may be a strict subset of
%% those in Apps.
find_incoming_app(#peer{apps = SApps, caps = Caps}, Ids, Apps) ->
    case keyfind(Ids, 1, SApps) of
        {_Id, Alias} ->
            {#diameter_app{} = find_app(Alias, Apps), Caps};
        false ->
            Caps
    end.

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

%% get_value/3

get_value(_, [], Def) ->
    Def;
get_value(Key, [L | Rest], Def) ->
    case lists:keyfind(Key, 1, L) of
        {_,V} ->
            V;
        _ ->
            get_value(Key, Rest, Def)
    end.

%% find_outgoing_app/2

find_outgoing_app(Alias, Apps) ->
    case find_app(Alias, Apps) of
        #diameter_app{id = ?APP_ID_RELAY} ->
            false;
        A ->
            A
    end.

%% find_app/2

find_app(Alias, Apps) ->
    lists:keyfind(Alias, #diameter_app.alias, Apps).

%% Don't bring down the service (and all associated connections)
%% regardless of what happens.
peer_cb(App, F, A) ->
    try state_cb(App, F, A) of
        ModS ->
            mod_state(App#diameter_app.alias, ModS),
            true
    catch
        E:R ->
            %% Don't include arguments since a #diameter_caps{} strings
            %% from the peer, which could be anything (especially, large).
            [Mod|X] = App#diameter_app.module,
            ?LOG(failure, {E, R, Mod, F, diameter_lib:get_stacktrace()}),
            error_report(failure, F, {Mod, F, A ++ X}),
            false
    end.

%% ---------------------------------------------------------------------------
%% # connection_down/3
%% ---------------------------------------------------------------------------

connection_down(#watchdog{state = ?WD_OKAY,
                          peer = TPid}
                = Wd,
                #peer{caps = Caps,
                      apps = SApps}
                = Pr,
                #state{service_name = SvcName,
                       service = #diameter_service{applications = Apps},
                       local_peers = LDict}
                = S) ->
    report_status(down, Wd, Pr, S, []),
    remove_local_peer(SApps, {{TPid, Caps}, {SvcName, Apps}}, LDict),
    diameter_traffic:peer_down(TPid);

connection_down(#watchdog{state = ?WD_OKAY,
                          peer = TPid}
                = Wd,
                To,
                #state{peerT = PeerT}
                = S)
  when is_atom(To) ->
    connection_down(Wd, #peer{} = fetch(PeerT, TPid), S);

connection_down(#watchdog{}, _, _) ->
    ok.

remove_local_peer(SApps, T, LDict) ->
    lists:foldl(fun(A,D) -> rlp(A, T, D) end, LDict, SApps).

rlp({Id, Alias}, {TC, SA}, LDict) ->
    L = ?Dict:fetch(Alias, LDict),
    down_conn(Id, Alias, TC, SA),
    ?Dict:store(Alias, lists:delete(TC, L), LDict).

down_conn(Id, Alias, TC, {SvcName, Apps}) ->
    #diameter_app{id = Id}  %% assert
        = App
        = find_app(Alias, Apps),

    peer_cb(App, peer_down, [SvcName, TC]).

%% ---------------------------------------------------------------------------
%% # watchdog_down/2
%% ---------------------------------------------------------------------------

%% Watchdog process has died.

watchdog_down(Pid, #state{watchdogT = WatchdogT} = S) ->
    Wd = fetch(WatchdogT, Pid),
    ets:delete_object(WatchdogT, Wd),
    restart(Wd,S),
    wd_down(Wd,S).

%% Watchdog has never reached OKAY ...
wd_down(#watchdog{peer = B}, _)
  when is_boolean(B) ->
    ok;

%% ... or maybe it has.
wd_down(#watchdog{peer = TPid} = Wd, #state{peerT = PeerT} = S) ->
    connection_down(Wd, ?WD_DOWN, S),
    ets:delete(PeerT, TPid).

%% restart/2

restart(Wd, S) ->
    q_restart(restart(Wd), S).

%% restart/1

%% Always try to reconnect.
restart(#watchdog{ref = Ref,
                  type = connect = T,
                  options = Opts,
                  started = Time}) ->
    {Time, {Ref, T, Opts}};

%% Transport connection hasn't yet been accepted ...
restart(#watchdog{ref = Ref,
                  type = accept = T,
                  options = Opts,
                  peer = false,
                  started = Time}) ->
    {Time, {Ref, T, Opts}};

%% ... or it has: a replacement has already been spawned.
restart(#watchdog{type = accept}) ->
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
    connect_timer(Opts, ?DEFAULT_TC);
default_tc(accept, _) ->
    0.

%% Accept both connect_timer and the (older) reconnect_timer, the
%% latter being a remnant from a time in which the timer did apply to
%% reconnect attempts.
connect_timer(Opts, Def0) ->
    Def = proplists:get_value(reconnect_timer, Opts, Def0),
    proplists:get_value(connect_timer, Opts, Def).

%% Bound tc below if the watchdog was restarted recently to avoid
%% continuous restarted in case of faulty config or other problems.
tc(Time, Tc) ->
    choose(Tc > ?RESTART_TC
             orelse diameter_lib:micro_diff(Time) > 1000*?RESTART_TC,
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

%% ---------------------------------------------------------------------------
%% # close/2
%% ---------------------------------------------------------------------------

%% The watchdog doesn't start a new fsm in the accept case, it
%% simply stays alive until someone tells it to die in order for
%% another watchdog to be able to detect that it should transition
%% from initial into reopen rather than okay. That someone is either
%% the accepting watchdog upon reception of a CER from the previously
%% connected peer, or us after connect_timer timeout or immediately.

close(#watchdog{type = connect}) ->
    ok;

close(#watchdog{type = accept,
                pid = Pid,
                options = Opts}) ->
    Tc = connect_timer(Opts, 2*?DEFAULT_TC),
    erlang:send_after(Tc, Pid, close).
%% The RFC's only document the behaviour of Tc, our connect_timer,
%% for the establishment of connections but we also give
%% connect_timer semantics for a listener, being the time within
%% which a new connection attempt is expected of a connecting peer.
%% The value should be greater than the peer's Tc + jitter.

%% ---------------------------------------------------------------------------
%% # reconnect/2
%% ---------------------------------------------------------------------------

reconnect(Pid, #state{service_name = SvcName,
                      watchdogT = WatchdogT}) ->
    #watchdog{ref = Ref,
              type = connect,
              options = Opts}
        = fetch(WatchdogT, Pid),
    send_event(SvcName, {reconnect, Ref, Opts}).

%% ---------------------------------------------------------------------------
%% # call_module/4
%% ---------------------------------------------------------------------------

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

cm([#diameter_app{alias = Alias} = App], Req, From, Svc) ->
    Args = [Req, From, Svc],

    try state_cb(App, handle_call, Args) of
        {noreply = T, ModS} ->
            mod_state(Alias, ModS),
            T;
        {reply = T, RC, ModS} ->
            mod_state(Alias, ModS),
            {T, RC};
        T ->
            ModX = App#diameter_app.module,
            ?LOG(invalid_return, {ModX, handle_call, Args, T}),
            invalid
    catch
        E: Reason ->
            ModX = App#diameter_app.module,
            Stack = diameter_lib:get_stacktrace(),
            ?LOG(failure, {E, Reason, ModX, handle_call, Stack}),
            failure
    end;

cm([], _, _, _) ->
    unknown;

cm([_,_|_], _, _, _) ->
    multiple.

%% ---------------------------------------------------------------------------
%% # report_status/5
%% ---------------------------------------------------------------------------

report_status(Status,
              #watchdog{ref = Ref,
                        peer = TPid,
                        type = Type,
                        options = Opts},
              #peer{apps = [_|_] = Apps,
                    caps = Caps},
              #state{service_name = SvcName}
              = S,
              Extra) ->
    share_peer(Status, Caps, Apps, TPid, S),
    Info = [Status, Ref, {TPid, Caps}, {type(Type), Opts} | Extra],
    send_event(SvcName, list_to_tuple(Info)).

%% send_event/2

send_event(SvcName, Info) ->
    send_event(#diameter_event{service = SvcName,
                               info = Info}).

send_event(#diameter_event{service = SvcName} = E) ->
    lists:foreach(fun({_, Pid}) -> Pid ! E end, subscriptions(SvcName)).

%% ---------------------------------------------------------------------------
%% # share_peer/5
%% ---------------------------------------------------------------------------

share_peer(up, Caps, Apps, TPid, #state{options = [_, {_,T} | _],
                                        service_name = Svc}) ->
    notify(T, Svc, {peer, TPid, [A || {_,A} <- Apps], Caps});

share_peer(_, _, _, _, _) ->
    ok.

%% ---------------------------------------------------------------------------
%% # share_peers/2
%% ---------------------------------------------------------------------------

share_peers(Pid, #state{options = [_, {_,T} | _], local_peers = PDict}) ->
    is_remote(Pid, T)
        andalso ?Dict:fold(fun(A,Ps,ok) -> sp(Pid, A, Ps), ok end, ok, PDict).

sp(Pid, Alias, Peers) ->
    lists:foreach(fun({P,C}) -> Pid ! {peer, P, [Alias], C} end, Peers).

is_remote(Pid, T) ->
    Node = node(Pid),
    Node /= node() andalso lists:member(Node, remotes(T)).

%% ---------------------------------------------------------------------------
%% # remote_peer_up/4
%% ---------------------------------------------------------------------------

remote_peer_up(Pid, Aliases, Caps, #state{options = [_, _, {_,T} | _]} = S) ->
    is_remote(Pid, T)
        andalso rpu(Pid, Aliases, Caps, S).

rpu(Pid, Aliases, Caps, #state{service = Svc, shared_peers = PDict}) ->
    #diameter_service{applications = Apps} = Svc,
    Key = #diameter_app.alias,
    F = fun(A) -> lists:keymember(A, Key, Apps) end,
    rpu(Pid, lists:filter(F, Aliases), Caps, PDict);

rpu(_, [] = No, _, _) ->
    No;
rpu(Pid, Aliases, Caps, PDict) ->
    erlang:monitor(process, Pid),
    T = {Pid, Caps},
    lists:foreach(fun(A) -> ?Dict:append(A, T, PDict) end, Aliases).

%% ---------------------------------------------------------------------------
%% # remote_peer_down/2
%% ---------------------------------------------------------------------------

remote_peer_down(Pid, #state{shared_peers = PDict}) ->
    lists:foreach(fun(A) -> rpd(Pid, A, PDict) end, ?Dict:fetch_keys(PDict)).

rpd(Pid, Alias, PDict) ->
    ?Dict:update(Alias, fun(Ps) -> lists:keydelete(Pid, 1, Ps) end, PDict).

%% ---------------------------------------------------------------------------
%% pick_peer/4
%% ---------------------------------------------------------------------------

pick_peer(#diameter_app{alias = Alias}
          = App,
          RealmAndHost,
          Filter,
          #state{local_peers = L,
                 shared_peers = S,
                 service_name = SvcName,
                 service = #diameter_service{pid = Pid}}) ->
    pick_peer(peers(Alias, RealmAndHost, Filter, L),
              peers(Alias, RealmAndHost, Filter, S),
              Pid,
              SvcName,
              App).

%% pick_peer/5

pick_peer([], [], _, _, _) ->
    false;

%% App state is mutable but we're not in the service process: go there.
pick_peer(Local, Remote, Pid, _SvcName, #diameter_app{mutable = true} = App)
  when self() /= Pid ->
    case call_service(Pid, {pick_peer, Local, Remote, App}) of
        {TPid, _} = T when is_pid(TPid) ->
            T;
        false = No ->
            No;
        {error, _} ->
            false
    end;

%% App state isn't mutable or it is and we're in the service process:
%% do the deed.
pick_peer(Local,
          Remote,
          _Pid,
          SvcName,
          #diameter_app{alias = Alias,
                        init_state = S,
                        mutable = M}
          = App) ->
    Args = [Local, Remote, SvcName],

    try state_cb(App, pick_peer, Args) of
        {ok, {TPid, #diameter_caps{}} = T} when is_pid(TPid) ->
            T;
        {{TPid, #diameter_caps{}} = T, ModS} when is_pid(TPid), M ->
            mod_state(Alias, ModS),
            T;
        {false = No, ModS} when M ->
            mod_state(Alias, ModS),
            No;
        {ok, false = No} ->
            No;
        false = No ->
            No;
        {{TPid, #diameter_caps{}} = T, S} when is_pid(TPid) ->
            T;                     %% Accept returned state in the immutable
        {false = No, S} ->         %% case as long it isn't changed.
            No;
        T when M ->
            ModX = App#diameter_app.module,
            ?LOG(invalid_return, {ModX, pick_peer, T}),
            false
    catch
        E: Reason when M ->
            ModX = App#diameter_app.module,
            Stack = diameter_lib:get_stacktrace(),
            ?LOG(failure, {E, Reason, ModX, pick_peer, Stack}),
            false
    end.

%% peers/4

peers(Alias, RH, Filter, Peers) ->
    case ?Dict:find(Alias, Peers) of
        {ok, L} ->
            filter(L, RH, Filter);
        error ->
            []
    end.

%% filter/3
%%
%% Return peers in match order.

filter(Peers, RH, Filter) ->
    {Ts, _} = fltr(Peers, RH, Filter),
    Ts.

%% fltr/4

fltr(Peers, _, none) ->
    {Peers, []};

fltr(Peers, RH, {neg, F}) ->
    {Ts, Fs} = fltr(Peers, RH, F),
    {Fs, Ts};

fltr(Peers, RH, {all, L})
  when is_list(L) ->
    lists:foldl(fun(F,A) -> fltr_all(F, A, RH) end,
                {Peers, []},
                L);

fltr(Peers, RH, {any, L})
  when is_list(L) ->
    lists:foldl(fun(F,A) -> fltr_any(F, A, RH) end,
                {[], Peers},
                L);

fltr(Peers, RH, F) ->
    lists:partition(fun({_,C}) -> caps_filter(C, RH, F) end, Peers).

fltr_all(F, {Ts0, Fs0}, RH) ->
    {Ts1, Fs1} = fltr(Ts0, RH, F),
    {Ts1, Fs0 ++ Fs1}.

fltr_any(F, {Ts0, Fs0}, RH) ->
    {Ts1, Fs1} = fltr(Fs0, RH, F),
    {Ts0 ++ Ts1, Fs1}.

%% caps_filter/3

caps_filter(#diameter_caps{origin_host = {_,OH}}, [_,DH], host) ->
    eq(undefined, DH, OH);

caps_filter(#diameter_caps{origin_realm = {_,OR}}, [DR,_], realm) ->
    eq(undefined, DR, OR);

caps_filter(C, _, Filter) ->
    caps_filter(C, Filter).

%% caps_filter/2

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

transports(#state{watchdogT = WatchdogT}) ->
    ets:select(WatchdogT, [{#watchdog{peer = '$1', _ = '_'},
                        [{'is_pid', '$1'}],
                        ['$1']}]).

%% ---------------------------------------------------------------------------
%% # service_info/2
%% ---------------------------------------------------------------------------

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
                     statistics,
                     info]).

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

tagged_info(TPid, #state{watchdogT = WatchdogT, peerT = PeerT})
  when is_pid(TPid) ->
    try
        [#peer{watchdog = Pid}] = ets:lookup(PeerT, TPid),
        [#watchdog{ref = Ref, type = Type, options = Opts}]
            = ets:lookup(WatchdogT, Pid),
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
        info         -> info_info(S);
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

info_stats(#state{watchdogT = WatchdogT}) ->
    MatchSpec = [{#watchdog{ref = '$1', peer = '$2', _ = '_'},
                  [{'is_pid', '$2'}],
                  [['$1', '$2']]}],
    try ets:select(WatchdogT, MatchSpec) of
        L ->
            diameter_stats:read(lists:append(L))
    catch
        error: badarg -> []  %% service  has gone down
    end.

%% info_transport/1
%%
%% One entry per configured transport. Statistics for each entry are
%% the accumulated values for the ref and associated watchdog/peer
%% pids.

info_transport(S) ->
    PeerD = peer_dict(S, config_dict(S)),
    Stats = diameter_stats:sum(dict:fetch_keys(PeerD)),
    dict:fold(fun(R, Ls, A) ->
                      Cs = proplists:get_value(R, Stats, []),
                      [[{ref, R} | transport(Ls)] ++ [{statistics, Cs}] | A]
              end,
              [],
              PeerD).

%% Single config entry. Distinguish between pool_size config or not on
%% a connecting transport for backwards compatibility: with the option
%% the form is similar to the listening case, with connections grouped
%% in a pool tuple (for lack of a better name), without as before.
transport([[{type, Type}, {options, Opts}] = L])
  when Type == listen;
       Type == connect ->
    L ++ [{K, []} || [{_,K}] <- [keys(Type, Opts)]];

%% Peer entries: discard config. Note that the peer entries have
%% length at least 3.
transport([[_,_] | L]) ->
    transport(L);

%% Multiple tranports. Note that all have the same options by
%% construction, which is not terribly space efficient.
transport([[{type, Type}, {options, Opts} | _] | _] = Ls) ->
    transport(keys(Type, Opts), Ls).

%% Group transports in an accept or pool tuple ...
transport([{Type, Key}], [[{type, _}, {options, Opts} | _] | _] = Ls) ->
    [{type, Type},
     {options, Opts},
     {Key, [tl(tl(L)) || L <- Ls]}];

%% ... or not: there can only be one.
transport([], [L]) ->
    L.

keys(connect = T, Opts) ->
    [{T, pool} || lists:keymember(pool_size, 1, Opts)];
keys(_, _) ->
    [{listen, accept}].

peer_dict(#state{watchdogT = WatchdogT, peerT = PeerT}, Dict0) ->
    try ets:tab2list(WatchdogT) of
        L -> lists:foldl(fun(T,A) -> peer_acc(PeerT, A, T) end, Dict0, L)
    catch
        error: badarg -> Dict0  %% service has gone down
    end.

peer_acc(PeerT, Acc, #watchdog{pid = Pid,
                               type = Type,
                               ref = Ref,
                               options = Opts,
                               state = WS,
                               started = At,
                               peer = TPid}) ->
    Info = [{type, Type},
            {options, Opts},
            {watchdog, {Pid, At, WS}}
            | info_peer(PeerT, TPid, WS)],
    dict:append(Ref, Info ++ [{info, info_process_info(Info)}], Acc).

info_peer(PeerT, TPid, WS)
  when is_pid(TPid), WS /= ?WD_DOWN ->
    try ets:lookup(PeerT, TPid) of
        T -> info_peer(T)
    catch
        error: badarg -> []  %% service has gone down
    end;
info_peer(_, _, _) ->
    [].

info_process_info(Info) ->
    lists:flatmap(fun ipi/1, Info).

ipi({watchdog, {Pid, _, _}}) ->
    info_pid(Pid);

ipi({peer, {Pid, _}}) ->
    info_pid(Pid);

ipi({port, [{owner, Pid} | _]}) ->
    info_pid(Pid);

ipi(_) ->
    [].

info_pid(Pid) ->
    case process_info(Pid, [message_queue_len, memory, binary]) of
        undefined ->
            [];
        L ->
            [{Pid, lists:map(fun({K,V}) -> {K, map_info(K,V)} end, L)}]
    end.

%% The binary list consists of 3-tuples {Ptr, Size, Count}, where Ptr
%% is a C pointer value, Size is the size of a referenced binary in
%% bytes, and Count is a global reference count. The same Ptr can
%% occur multiple times, once for each reference on the process heap.
%% In this case, the corresponding tuples will have Size in common but
%% Count may differ just because no global lock is taken when the
%% value is retrieved.
%%
%% The list can be quite large, and we aren't often interested in the
%% pointers or counts, so whittle this down to the number of binaries
%% referenced and their total byte count.
map_info(binary, L) ->
    SzD = lists:foldl(fun({P,S,_}, D) -> dict:store(P,S,D) end,
                      dict:new(),
                      L),
    {dict:size(SzD), dict:fold(fun(_,S,N) -> S + N end, 0, SzD)};

map_info(_, T) ->
    T.

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

info_peer([#peer{pid = Pid, apps = SApps, caps = Caps, started = T}]) ->
    [{peer, {Pid, T}},
     {apps, SApps},
     {caps, info_caps(Caps)}
     | try [{port, info_port(Pid)}] catch _:_ -> [] end];
info_peer([] = No) ->
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
    diameter_traffic:pending(transports(S)).

%% info_info/1
%%
%% Extract process_info from connections info.

info_info(S) ->
    [I || L <- conn_list(S), {info, I} <- L].

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
