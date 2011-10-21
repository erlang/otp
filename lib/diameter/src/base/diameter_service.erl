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
         whois/1,
         flush_stats/1]).

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
-include("diameter_types.hrl").

-define(STATE_UP,   up).
-define(STATE_DOWN, down).

-define(DEFAULT_TC,     30000).  %% RFC 3588 ch 2.1
-define(DEFAULT_TIMEOUT, 5000).  %% for outgoing requests
-define(RESTART_TC,      1000).  %% if restart was this recent

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

%% Workaround for dialyzer's lack of understanding of match specs.
-type match(T)
   :: T | '_' | '$1' | '$2' | '$3' | '$4'.

%% State of service gen_server.
-record(state,
        {id = now(),
         service_name,      %% as passed to start_service/2, key in ?STATE_TABLE
         service :: #diameter_service{},
         peerT = ets_new(peers) :: ets:tid(), %% #peer{} at start_fsm
         connT = ets_new(conns) :: ets:tid(), %% #conn{} at connection_up
         share_peers = false :: boolean(), %% broadcast peers to remote nodes?
         use_shared_peers = false :: boolean(),  %% use broadcasted peers?
         shared_peers = ?Dict:new(),         %% Alias -> [{TPid, Caps}, ...]
         local_peers = ?Dict:new(),          %% Alias -> [{TPid, Caps}, ...]
         monitor = false :: false | pid()}). %% process to die with
%% shared_peers reflects the peers broadcast from remote nodes. Note
%% that the state term itself doesn't change, which is relevant for
%% the stateless application callbacks since the state is retrieved
%% from ?STATE_TABLE from outside the service process. The pid in the
%% service record is used to determine whether or not we need to call
%% the process for a pick_peer callback.

%% Record representing a watchdog process.
-record(peer,
        {pid  :: match(pid()),
         type :: match(connect | accept),
         ref  :: match(reference()),  %% key into diameter_config
         options :: match([transport_opt()]), %% as passed to start_transport
         op_state = ?STATE_DOWN :: match(?STATE_DOWN | ?STATE_UP),
         started = now(),      %% at process start
         conn = false :: match(boolean() | pid())}).
                      %% true at accept, pid() at connection_up (connT key)

%% Record representing a peer_fsm process.
-record(conn,
        {pid   :: pid(),
         apps  :: [{0..16#FFFFFFFF, app_alias()}], %% {Id, Alias}
         caps  :: #diameter_caps{},
         started = now(),  %% at process start
         peer  :: pid()}). %% key into peerT

%% Record stored in diameter_request for each outgoing request.
-record(request,
        {from,               %% arg 2 of handle_call/3
         handler    :: match(pid()), %% request process
         transport  :: match(pid()), %% peer process
         caps       :: match(#diameter_caps{}),
         app        :: match(app_alias()),  %% #diameter_app.alias
         dictionary :: match(module()),     %% #diameter_app.dictionary
         module     :: match(nonempty_improper_list(module(), list())),
                    %% #diameter_app.module
         filter     :: match(peer_filter()),
         packet     :: match(#diameter_packet{})}).

%% Record call/4 options are parsed into.
-record(options,
        {filter = none  :: peer_filter(),
         extra = []     :: list(),
         timeout = ?DEFAULT_TIMEOUT :: 0..16#FFFFFFFF,
         detach = false :: boolean()}).

%% Since RFC 3588 requires that a Diameter agent not modify End-to-End
%% Identifiers, the possibility of explicitly setting an End-to-End
%% Identifier would be needed to be able to implement an agent in
%% which one side of the communication is not implemented on top of
%% diameter. For example, Diameter being sent or received encapsulated
%% in some other protocol, or even another Diameter stack in a
%% non-Erlang environment. (Not that this is likely to be a normal
%% case.)
%%
%% The implemented solution is not an option but to respect any header
%% values set in a diameter_header record returned from a
%% prepare_request callback. A call to diameter:call/4 can communicate
%% values to the callback using the 'extra' option if so desired.

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
    info_rc(call_service_by_name(SvcName, {info, Item})).

info_rc({error, _}) ->
    undefined;
info_rc(Info) ->
    Info.

%%% ---------------------------------------------------------------------------
%%% # receive_message(TPid, Pkt, MessageData)
%%% ---------------------------------------------------------------------------

%% Handle an incoming message in the watchdog process. This used to
%% come through the service process but this avoids that becoming a
%% bottleneck.

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
    c(ets:lookup(?STATE_TABLE, SvcName), App, Msg, Opts, Caller).

c([#state{service_name = SvcName} = S], App, Msg, Opts, Caller) ->
    case find_transport(App, Msg, Opts, S) of
        {_,_,_} = T ->
            send_request(T, Msg, Opts, Caller, SvcName);
        false ->
            {error, no_connection};
        {error, _} = No ->
            No
    end;

c([], _, _, _, _) ->
    {error, no_service}.

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

%%% ---------------------------------------------------------------------------
%%% # flush_stats/1
%%%
%%% Output: list of {{SvcName, Alias, Counter}, Value}
%%% ---------------------------------------------------------------------------

flush_stats(TPid) ->
    diameter_stats:flush(TPid).

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

handle_call({info, Item}, _From, S) ->
    {reply, service_info(Item, S), S};

handle_call(stop, _From, S) ->
    shutdown(S),
    {stop, normal, ok, S};
%% The server currently isn't guaranteed to be dead when the caller
%% gets the reply. We deal with this in the call to the server,
%% stating a monitor that waits for DOWN before returning.

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

handle_info(T,S) ->
    case transition(T,S) of
        ok ->
            {noreply, S};
        #state{} = NS ->
            {noreply, NS};
        {stop, Reason} ->
            {stop, {shutdown, Reason}, S}
    end.

%% transition/2

%% Peer process is telling us to start a new accept process.
transition({accepted, Pid, TPid}, S) ->
    accepted(Pid, TPid, S),
    ok;

%% Peer process has a new open connection.
transition({connection_up, Pid, T}, S) ->
    connection_up(Pid, T, S);

%% Peer process has left state open.
transition({connection_down, Pid}, S) ->
    connection_down(Pid, S);

%% Peer process has returned to state open.
transition({connection_up, Pid}, S) ->
    connection_up(Pid, S);

%% Accepting transport has lost connectivity.
transition({close, Pid}, S) ->
    close(Pid, S),
    ok;

%% Connecting transport is being restarted by watchdog.
transition({reconnect, Pid}, S) ->
    reconnect(Pid, S),
    ok;

%% Monitor process has died. Just die with a reason that tells
%% diameter_config about the happening. If a cleaner shutdown is
%% required then someone should stop us.
transition({'DOWN', MRef, process, _, Reason}, #state{monitor = MRef}) ->
    {stop, {monitor, Reason}};

%% Local peer process has died.
transition({'DOWN', _, process, Pid, Reason}, S)
  when node(Pid) == node() ->
    peer_down(Pid, Reason, S);

%% Remote service wants to know about shared transports.
transition({service, Pid}, S) ->
    share_peers(Pid, S),
    ok;

%% Remote service is communicating a shared peer.
transition({peer, TPid, Aliases, Caps}, S) ->
    remote_peer_up(TPid, Aliases, Caps, S);

%% Remote peer process has died.
transition({'DOWN', _, process, TPid, _}, S) ->
    remote_peer_down(TPid, S);

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

transition(Req, S) ->
    unexpected(handle_info, [Req], S),
    ok.

%%% ---------------------------------------------------------------------------
%%% # terminate(Reason, State)
%%% ---------------------------------------------------------------------------

terminate(Reason, #state{service_name = Name} = S) ->
    ets:delete(?STATE_TABLE, Name),
    shutdown == Reason  %% application shutdown
        andalso shutdown(S).

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

monitor(Pid) ->
    erlang:monitor(process, Pid),
    Pid.

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

%% have_transport/2

have_transport(SvcName, Ref) ->
    [] /= diameter_config:have_transport(SvcName, Ref).

%%% ---------------------------------------------------------------------------
%%% # shutdown/2
%%% ---------------------------------------------------------------------------

shutdown(Refs, #state{peerT = PeerT}) ->
    ets:foldl(fun(P,ok) -> s(P, Refs), ok end, ok, PeerT).

s(#peer{ref = Ref, pid = Pid}, Refs) ->
    s(lists:member(Ref, Refs), Pid);

s(true, Pid) ->
    Pid ! {shutdown, self()};  %% 'DOWN' will cleanup as usual
s(false, _) ->
    ok.

%%% ---------------------------------------------------------------------------
%%% # shutdown/1
%%% ---------------------------------------------------------------------------

shutdown(#state{peerT = PeerT}) ->
    %% A transport might not be alive to receive the shutdown request
    %% but give those that are a chance to shutdown gracefully.
    wait(fun st/2, PeerT),
    %% Kill the watchdogs explicitly in case there was no transport.
    wait(fun sw/2, PeerT).

wait(Fun, T) ->
    diameter_lib:wait(ets:foldl(Fun, [], T)).

st(#peer{conn = B}, Acc)
  when is_boolean(B) ->
    Acc;
st(#peer{conn = Pid}, Acc) ->
    Pid ! shutdown,
    [Pid | Acc].

sw(#peer{pid = Pid}, Acc) ->
    exit(Pid, shutdown),
    [Pid | Acc].

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
    S.

cfg_acc({SvcName, #diameter_service{applications = Apps} = Rec, Opts},
        {false, Acc}) ->
    lists:foreach(fun init_mod/1, Apps),
    S = #state{service_name = SvcName,
               service = Rec#diameter_service{pid = self()},
               share_peers = get_value(share_peers, Opts),
               use_shared_peers = get_value(use_shared_peers, Opts),
               monitor = mref(get_value(monitor, Opts))},
    {S, Acc};

cfg_acc({_Ref, Type, _Opts} = T, {S, Acc})
  when Type == connect;
       Type == listen ->
    {S, [T | Acc]}.

mref(false = No) ->
    No;
mref(P) ->
    erlang:monitor(process, P).

init_shared(#state{use_shared_peers = true,
                   service_name = Svc}) ->
    diameter_peer:notify(Svc, {service, self()});
init_shared(#state{use_shared_peers = false}) ->
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
    Pid = monitor(s(Type, Ref, {ConnT,
                                Opts,
                                SvcName,
                                merge_service(Opts, Svc)})),
    insert(PeerT, #peer{pid = Pid,
                        type = Type,
                        ref = Ref,
                        options = Opts}),
    Pid.

%% Note that the service record passed into the watchdog is the merged
%% record so that each watchdog (and peer_fsm) may get a different
%% record. This record is what is passed back into application
%% callbacks.

s(Type, Ref, T) ->
    diameter_watchdog:start({Type, Ref}, T).

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
    insert(PeerT, P#peer{conn = true}),  %% mark replacement transport started
    start(Ref, T, Opts, S).              %% start new peer

fetch(Tid, Key) ->
    [T] = ets:lookup(Tid, Key),
    T.

%%% ---------------------------------------------------------------------------
%%% # connection_up/3
%%%
%%% Output: #state{}
%%% ---------------------------------------------------------------------------

%% Peer process has reached the open state.

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
%%% # connection_up/2
%%%
%%% Output: #state{}
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
    #peer{conn = TPid, op_state = ?STATE_DOWN}
        = P,
    #conn{apps = SApps, caps = Caps}
        = C,

    insert(PeerT, P#peer{op_state = ?STATE_UP}),

    request_peer_up(TPid),
    report_status(up, P, C, S, T),
    S#state{local_peers = insert_local_peer(SApps,
                                            {{TPid, Caps}, {SvcName, Apps}},
                                            LDict)}.

insert_local_peer(SApps, T, LDict) ->
    lists:foldl(fun(A,D) -> ilp(A, T, D) end, LDict, SApps).

ilp({Id, Alias}, {TC, SA}, LDict) ->
    init_conn(Id, Alias, TC, SA),
    ?Dict:append(Alias, TC, LDict).

init_conn(Id, Alias, TC, {SvcName, Apps}) ->
    #diameter_app{module = ModX,
                  id = Id}  %% assert
        = find_app(Alias, Apps),

    peer_cb({ModX, peer_up, [SvcName, TC]}, Alias).

find_app(Alias, Apps) ->
    lists:keyfind(Alias, #diameter_app.alias, Apps).

%% A failing peer callback brings down the service. In the case of
%% peer_up we could just kill the transport and emit an error but for
%% peer_down we have no way to cleanup any state change that peer_up
%% may have introduced.
peer_cb(MFA, Alias) ->
    try state_cb(MFA, Alias) of
        ModS ->
            mod_state(Alias, ModS)
    catch
        E: Reason ->
            ?ERROR({E, Reason, MFA, ?STACK})
    end.

%%% ---------------------------------------------------------------------------
%%% # connection_down/2
%%%
%%% Output: #state{}
%%% ---------------------------------------------------------------------------

%% Peer process has transitioned out of the open state.

connection_down(Pid, #state{peerT = PeerT,
                            connT = ConnT}
                     = S) ->
    #peer{conn = TPid}
        = P
        = fetch(PeerT, Pid),

    C = fetch(ConnT, TPid),
    insert(PeerT, P#peer{op_state = ?STATE_DOWN}),
    connection_down(P,C,S).

%% connection_down/3

connection_down(#peer{conn = TPid,
                      op_state = ?STATE_UP}
                = P,
                #conn{caps = Caps,
                      apps = SApps}
                = C,
                #state{service_name = SvcName,
                       service = #diameter_service{applications = Apps},
                       local_peers = LDict}
                = S) ->
    report_status(down, P, C, S, []),
    NewS = S#state{local_peers
                   = remove_local_peer(SApps,
                                       {{TPid, Caps}, {SvcName, Apps}},
                                       LDict)},
    request_peer_down(TPid, NewS),
    NewS.

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
%%%
%%% Output: #state{}
%%% ---------------------------------------------------------------------------

%% Peer process has died.

peer_down(Pid, _Reason, #state{peerT = PeerT} = S) ->
    P = fetch(PeerT, Pid),
    ets:delete_object(PeerT, P),
    restart(P,S),
    peer_down(P,S).

%% peer_down/2

%% The peer has never come up ...
peer_down(#peer{conn = B}, S)
  when is_boolean(B) ->
    S;

%% ... or it has.
peer_down(#peer{ref = Ref,
                conn = TPid,
                type = Type,
                options = Opts}
          = P,
          #state{service_name = SvcName,
                 connT = ConnT}
          = S) ->
    #conn{caps = Caps}
        = C
        = fetch(ConnT, TPid),
    ets:delete_object(ConnT, C),
    try
        pd(P,C,S)
    after
        send_event(SvcName, {closed, Ref, {TPid, Caps}, {type(Type), Opts}})
    end.

pd(#peer{op_state = ?STATE_DOWN}, _, S) ->
    S;
pd(#peer{op_state = ?STATE_UP} = P, C, S) ->
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

%% ... or it has: a replacement transport has already been spawned.
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

%% Bound tc below if the peer was restarted recently to avoid
%% continuous in case of faulty config or other problems.
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
    tc(have_transport(SvcName, Ref), T, S).

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

    c(Pid, have_transport(SvcName, Ref), Opts).

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
%%% # send_request/5
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

send_request({TPid, Caps, App}, Msg, Opts, Caller, SvcName) ->
    #diameter_app{module = ModX}
        = App,

    Pkt = make_packet(Msg),

    case cb(ModX, prepare_request, [Pkt, SvcName, {TPid, Caps}]) of
        {send, P} ->
            send_request(make_packet(P, Pkt),
                         TPid,
                         Caps,
                         App,
                         Opts,
                         Caller,
                         SvcName);
        {discard, Reason} ->
            {error, Reason};
        discard ->
            {error, discarded};
        T ->
            ?ERROR({invalid_return, prepare_request, App, T})
    end.

%% make_packet/1
%%
%% Turn an outgoing request as passed to call/4 into a diameter_packet
%% record in preparation for a prepare_request callback.

make_packet(Bin)
  when is_binary(Bin) ->
    #diameter_packet{header = diameter_codec:decode_header(Bin),
                     bin = Bin};

make_packet(#diameter_packet{msg = [#diameter_header{} = Hdr | Avps]} = Pkt) ->
    Pkt#diameter_packet{msg = [make_header(Hdr) | Avps]};

make_packet(#diameter_packet{header = Hdr} = Pkt) ->
    Pkt#diameter_packet{header = make_header(Hdr)};

make_packet(Msg) ->
    make_packet(#diameter_packet{msg = Msg}).

%% make_header/1

make_header(undefined) ->
    Seq = diameter_session:sequence(),
    make_header(#diameter_header{end_to_end_id = Seq,
                                 hop_by_hop_id = Seq});

make_header(#diameter_header{version = undefined} = Hdr) ->
    make_header(Hdr#diameter_header{version = ?DIAMETER_VERSION});

make_header(#diameter_header{end_to_end_id = undefined} = H) ->
    Seq = diameter_session:sequence(),
    make_header(H#diameter_header{end_to_end_id = Seq});

make_header(#diameter_header{hop_by_hop_id = undefined} = H) ->
    Seq = diameter_session:sequence(),
    make_header(H#diameter_header{hop_by_hop_id = Seq});

make_header(#diameter_header{} = Hdr) ->
    Hdr;

make_header(T) ->
    ?ERROR({invalid_header, T}).

%% make_packet/2
%%
%% Reconstruct a diameter_packet from the return value of
%% prepare_request or prepare_retransmit callback.

make_packet(Bin, _)
  when is_binary(Bin) ->
    make_packet(Bin);

make_packet(#diameter_packet{msg = [#diameter_header{} | _]} = Pkt, _) ->
    Pkt;

%% Returning a diameter_packet with no header from a prepare_request
%% or prepare_retransmit callback retains the header passed into it.
%% This is primarily so that the end to end and hop by hop identifiers
%% are retained.
make_packet(#diameter_packet{header = Hdr} = Pkt,
            #diameter_packet{header = Hdr0}) ->
    Pkt#diameter_packet{header = fold_record(Hdr0, Hdr)};

make_packet(Msg, Pkt) ->
    Pkt#diameter_packet{msg = Msg}.

%% fold_record/2

fold_record(undefined, R) ->
    R;
fold_record(Rec, R) ->
    diameter_lib:fold_tuple(2, Rec, R).

%% send_request/7

send_request(Pkt, TPid, Caps, App, Opts, Caller, SvcName) ->
    #diameter_app{alias = Alias,
                  dictionary = Dict,
                  module = ModX,
                  answer_errors = AE}
        = App,

    EPkt = encode(Dict, Pkt),

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

retransmit({TPid, Caps, #diameter_app{alias = Alias} = App},
           #request{app = Alias,
                    packet = Pkt}
           = Req,
           SvcName,
           Timeout) ->
    have_request(Pkt, TPid)      %% Don't failover to a peer we've
        andalso ?THROW(timeout), %% already sent to.

    case cb(App, prepare_retransmit, [Pkt, SvcName, {TPid, Caps}]) of
        {send, P} ->
            retransmit(make_packet(P, Pkt), TPid, Caps, Req, Timeout);
        {discard, Reason} ->
            ?THROW(Reason);
        discard ->
            ?THROW(discarded);
        T ->
            ?ERROR({invalid_return, prepare_retransmit, App, T})
    end.

%% retransmit/5

retransmit(Pkt, TPid, Caps, #request{dictionary = Dict} = Req, Timeout) ->
    EPkt = encode(Dict, Pkt),

    NewReq = Req#request{transport = TPid,
                         packet = Pkt,
                         caps = Caps},

    ?LOG(retransmission, NewReq),
    TRef = send_request(TPid, EPkt, NewReq, Timeout),
    {TRef, NewReq}.

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

recv_request(TPid, Pkt, {ConnT, SvcName, Apps}) ->
    try ets:lookup(ConnT, TPid) of
        [C] ->
            recv_request(C, TPid, Pkt, SvcName, Apps);
        [] ->             %% transport has gone down
            ok
    catch
        error: badarg ->  %% service has gone down (and taken table with it)
            ok
    end.

%% recv_request/5

recv_request(#conn{apps = SApps, caps = Caps}, TPid, Pkt, SvcName, Apps) ->
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,

    #diameter_packet{header = #diameter_header{application_id = Id}}
        = Pkt,

    recv_request(find_recv_app(Id, SApps),
                 {SvcName, OH, OR},
                 TPid,
                 Apps,
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

%% recv_request/6

recv_request({Id, Alias}, T, TPid, Apps, Caps, Pkt) ->
    #diameter_app{dictionary = Dict}
        = A
        = find_app(Alias, Apps),
    recv_request(T, {TPid, Caps}, A, diameter_codec:decode(Id, Dict, Pkt));
%% Note that the decode is different depending on whether or not Id is
%% ?APP_ID_RELAY.

%%   DIAMETER_APPLICATION_UNSUPPORTED   3007
%%      A request was sent for an application that is not supported.

recv_request(false, T, TPid, _, _, Pkt) ->
    As = collect_avps(Pkt),
    protocol_error(3007, T, TPid, Pkt#diameter_packet{avps = As}).

collect_avps(Pkt) ->
    case diameter_codec:collect_avps(Pkt) of
        {_Bs, As} ->
            As;
        As ->
            As
    end.

%% recv_request/4

%% Wrong number of bits somewhere in the message: reply.
%%
%%   DIAMETER_INVALID_AVP_BITS          3009
%%      A request was received that included an AVP whose flag bits are
%%      set to an unrecognized value, or that is inconsistent with the
%%      AVP's definition.
%%
recv_request(T, {TPid, _}, _, #diameter_packet{errors = [Bs | _]} = Pkt)
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
             #diameter_packet{header = #diameter_header{is_error = true}}
             = Pkt) ->
    protocol_error(3008, T, TPid, Pkt);

%% A message in a locally supported application or a proxiable message
%% in the relay application. Don't distinguish between the two since
%% each application has its own callback config. That is, the user can
%% easily distinguish between the two cases.
recv_request(T, TC, App, Pkt) ->
    request_cb(T, TC, App, examine(Pkt)).

%% Note that there may still be errors but these aren't protocol
%% (3xxx) errors that lead to an answer-message.

request_cb({SvcName, _OH, _OR} = T, TC, App, Pkt) ->
    request_cb(cb(App, handle_request, [Pkt, SvcName, TC]), App, T, TC, Pkt).

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

%% request_cb/5

%% A reply may be an answer-message, constructed either here or by
%% the handle_request callback. The header from the incoming request
%% is passed into the encode so that it can retrieve the relevant
%% command code in this case. It will also then ignore Dict and use
%% the base encoder.
request_cb({reply, Ans},
           #diameter_app{dictionary = Dict},
           _,
           {TPid, _},
           Pkt) ->
    reply(Ans, Dict, TPid, Pkt);

%% An 3xxx result code, for which the E-bit is set in the header.
request_cb({protocol_error, RC}, _, T, {TPid, _}, Pkt)
  when 3000 =< RC, RC < 4000 ->
    protocol_error(RC, T, TPid, Pkt);

%% RFC 3588 says we must reply 3001 to anything unrecognized or
%% unsupported. 'noreply' is undocumented (and inappropriately named)
%% backwards compatibility for this, protocol_error the documented
%% alternative.
request_cb(noreply, _, T, {TPid, _}, Pkt) ->
    protocol_error(3001, T, TPid, Pkt);

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
           T,
           TC,
           Pkt)
  when A == relay, Id == ?APP_ID_RELAY;
       A == proxy, Id /= ?APP_ID_RELAY;
       A == resend ->
    resend(Opts, App, T, TC, Pkt);

request_cb(discard, _, _, _, _) ->
    ok;

request_cb({eval, RC, F}, App, T, TC, Pkt) ->
    request_cb(RC, App, T, TC, Pkt),
    diameter_lib:eval(F).

%% protocol_error/4

protocol_error(RC, {_, OH, OR}, TPid, #diameter_packet{avps = Avps} = Pkt) ->
    ?LOG({error, RC}, Pkt),
    reply(answer_message({OH, OR, RC}, Avps), ?BASE, TPid, Pkt).

%% resend/5
%%
%% Resend a message as a relay or proxy agent.

resend(Opts,
       #diameter_app{} = App,
       {_SvcName, OH, _OR} = T,
       {_TPid, _Caps} = TC,
       #diameter_packet{avps = Avps} = Pkt) ->
    {Code, _Flags, Vid} = ?BASE:avp_header('Route-Record'),
    resend(is_loop(Code, Vid, OH, Avps), Opts, App, T, TC, Pkt).

%%   DIAMETER_LOOP_DETECTED             3005
%%      An agent detected a loop while trying to get the message to the
%%      intended recipient.  The message MAY be sent to an alternate peer,
%%      if one is available, but the peer reporting the error has
%%      identified a configuration problem.

resend(true, _, _, T, {TPid, _}, Pkt) ->  %% Route-Record loop
    protocol_error(3005, T, TPid, Pkt);

%% 6.1.8.  Relaying and Proxying Requests
%%
%%   A relay or proxy agent MUST append a Route-Record AVP to all requests
%%   forwarded.  The AVP contains the identity of the peer the request was
%%   received from.

resend(false,
       Opts,
       App,
       {SvcName, _, _} = T,
       {TPid, #diameter_caps{origin_host = {_, OH}}},
       #diameter_packet{header = Hdr0,
                        avps = Avps}
       = Pkt) ->
    Route = #diameter_avp{data = {?BASE, 'Route-Record', OH}},
    Seq = diameter_session:sequence(),
    Hdr = Hdr0#diameter_header{hop_by_hop_id = Seq},
    Msg = [Hdr, Route | Avps],
    resend(call(SvcName, App, Msg, Opts), T, TPid, Pkt).
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
       #diameter_packet{header = #diameter_header{hop_by_hop_id = Id},
                        transport_data = TD}) ->
    send(TPid, Pkt#diameter_packet{bin = diameter_codec:hop_by_hop_id(Id, B),
                                   transport_data = TD});
%% TODO: counters

%% Or not: DIAMETER_UNABLE_TO_DELIVER.
resend(_, T, TPid, Pkt) ->
    protocol_error(3002, T, TPid, Pkt).

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

%% reply/4
%%
%% Send a locally originating reply.

%% No errors or a diameter_header/avp list.
reply(Msg, Dict, TPid, #diameter_packet{errors = Es,
                                        transport_data = TD}
                       = ReqPkt)
  when [] == Es;
       is_record(hd(Msg), diameter_header) ->
    Pkt = diameter_codec:encode(Dict, make_reply_packet(Msg, ReqPkt)),
    incr(send, Pkt, Dict, TPid),  %% count result codes in sent answers
    send(TPid, Pkt#diameter_packet{transport_data = TD});

%% Or not: set Result-Code and Failed-AVP AVP's.
reply(Msg, Dict, TPid, #diameter_packet{errors = [H|_] = Es} = Pkt) ->
    reply(rc(Msg, rc(H), [A || {_,A} <- Es], Dict),
          Dict,
          TPid,
          Pkt#diameter_packet{errors = []}).

%% make_reply_packet/2

%% Binaries and header/avp lists are sent as-is.
make_reply_packet(Bin, _)
  when is_binary(Bin) ->
    #diameter_packet{bin = Bin};
make_reply_packet([#diameter_header{} | _] = Msg, _) ->
    #diameter_packet{msg = Msg};

%% Otherwise a reply message clears the R and T flags and retains the
%% P flag. The E flag will be set at encode.
make_reply_packet(Msg, #diameter_packet{header = ReqHdr}) ->
    Hdr = ReqHdr#diameter_header{version = ?DIAMETER_VERSION,
                                 is_request = false,
                                 is_error = undefined,
                                 is_retransmitted = false},
    #diameter_packet{header = Hdr,
                     msg = Msg}.

%% rc/1

rc({RC, _}) ->
    RC;
rc(RC) ->
    RC.

%% rc/4

rc(Rec, RC, Failed, Dict)
  when is_integer(RC) ->
    set(Rec, [{'Result-Code', RC} | failed_avp(Rec, Failed, Dict)], Dict).

%% Reply as name and tuple list ...
set([_|_] = Ans, Avps, _) ->
    Ans ++ Avps;  %% Values nearer tail take precedence.

%% ... or record.
set(Rec, Avps, Dict) ->
    Dict:'#set-'(Avps, Rec).

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
    a(examine(diameter_codec:decode(Dict, Pkt)),
      SvcName,
      AnswerErrors,
      Req).

%% We don't really need to do a full decode if we're a relay and will
%% just resend with a new hop by hop identifier, but might a proxy
%% want to examine the answer?

a(#diameter_packet{errors = []}
  = Pkt,
  SvcName,
  AE,
  #request{transport = TPid,
           dictionary = Dict,
           caps = Caps,
           packet = P}
  = Req) ->
    try
        incr(in, Pkt, Dict, TPid)
    of
        _ ->
            cb(Req, handle_answer, [Pkt, msg(P), SvcName, {TPid, Caps}])
    catch
        exit: {invalid_error_bit, _} = E ->
            e(Pkt#diameter_packet{errors = [E]}, SvcName, AE, Req)
    end;

a(#diameter_packet{} = Pkt, SvcName, AE, Req) ->
    e(Pkt, SvcName, AE, Req).

e(Pkt, SvcName, callback, #request{transport = TPid,
                                   caps = Caps,
                                   packet = Pkt}
                          = Req) ->
    cb(Req, handle_answer, [Pkt, msg(Pkt), SvcName, {TPid, Caps}]);
e(Pkt, SvcName, report, Req) ->
    x(errors, handle_answer, [SvcName, Req, Pkt]);
e(Pkt, SvcName, discard, Req) ->
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

incr(Dir, Pkt, Dict, TPid)
  when is_pid(TPid) ->
    #diameter_packet{header = #diameter_header{is_error = E}
                            = Hdr,
                     msg = Rec}
        = Pkt,

    D  = choose(E, ?BASE, Dict),
    RC = int(get_avp_value(D, 'Result-Code', Rec)),
    PE = is_protocol_error(RC),

    %% Check that the E bit is set only for 3xxx result codes.
    (not (E orelse PE))
        orelse (E andalso PE)
        orelse x({invalid_error_bit, RC}, answer, [Dir, Pkt]),

    Ctr = rc_counter(D, Rec, RC),
    is_tuple(Ctr)
        andalso incr(TPid, {diameter_codec:msg_id(Hdr), Dir, Ctr}).

%% incr/2

incr(TPid, Counter) ->
    diameter_stats:incr(Counter, TPid, 1).

%% RFC 3588, 7.6:
%%
%%   All Diameter answer messages defined in vendor-specific
%%   applications MUST include either one Result-Code AVP or one
%%   Experimental-Result AVP.
%%
%% Maintain statistics assuming one or the other, not both, which is
%% surely the intent of the RFC.

rc_counter(_, _, RC)
  when is_integer(RC) ->
    {'Result-Code', RC};
rc_counter(D, Rec, _) ->
    rcc(get_avp_value(D, 'Experimental-Result', Rec)).

%% Outgoing answers may be in any of the forms messages can be sent
%% in. Incoming messages will be records. We're assuming here that the
%% arity of the result code AVP's is 0 or 1.

rcc([{_,_,RC} = T])
  when is_integer(RC) ->
    T;
rcc({_,_,RC} = T)
  when is_integer(RC) ->
    T;
rcc(_) ->
    undefined.

int([N])
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
rt(#request{packet = #diameter_packet{msg = Msg}, dictionary = D} = Req, S) ->
    find_transport(get_destination(Msg, D), Req, S).

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

share_peer(up, Caps, Aliases, TPid, #state{share_peers = true,
                                           service_name = Svc}) ->
    diameter_peer:notify(Svc, {peer, TPid, Aliases, Caps});

share_peer(_, _, _, _, _) ->
    ok.

%%% ---------------------------------------------------------------------------
%%% # share_peers/2
%%% ---------------------------------------------------------------------------

share_peers(Pid, #state{share_peers = true,
                        local_peers = PDict}) ->
    ?Dict:fold(fun(A,Ps,ok) -> sp(Pid, A, Ps), ok end, ok, PDict);

share_peers(_, #state{share_peers = false}) ->
    ok.

sp(Pid, Alias, Peers) ->
    lists:foreach(fun({P,C}) -> Pid ! {peer, P, [Alias], C} end, Peers).

%%% ---------------------------------------------------------------------------
%%% # remote_peer_up/4
%%% ---------------------------------------------------------------------------

remote_peer_up(Pid, Aliases, Caps, #state{use_shared_peers = true,
                                          service = Svc,
                                          shared_peers = PDict}
                                   = S) ->
    #diameter_service{applications = Apps} = Svc,
    Update = lists:filter(fun(A) ->
                                  lists:keymember(A, #diameter_app.alias, Apps)
                          end,
                          Aliases),
    S#state{shared_peers = rpu(Pid, Caps, PDict, Update)};

remote_peer_up(_, _, _, #state{use_shared_peers = false} = S) ->
    S.

rpu(_, _, PDict, []) ->
    PDict;
rpu(Pid, Caps, PDict, Aliases) ->
    erlang:monitor(process, Pid),
    T = {Pid, Caps},
    lists:foldl(fun(A,D) -> ?Dict:append(A, T, D) end,
                PDict,
                Aliases).

%%% ---------------------------------------------------------------------------
%%% # remote_peer_down/2
%%% ---------------------------------------------------------------------------

remote_peer_down(Pid, #state{use_shared_peers = true,
                             shared_peers = PDict}
                      = S) ->
    S#state{shared_peers = lists:foldl(fun(A,D) -> rpd(Pid, A, D) end,
                                       PDict,
                                       ?Dict:fetch_keys(PDict))}.

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

ft(#diameter_app{module = Mod, dictionary = D} = App, Msg, Opts, S) ->
    #options{filter = Filter,
             extra = Xtra}
        = Opts,
    pick_peer(App#diameter_app{module = Mod ++ Xtra},
              get_destination(Msg, D),
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

get_destination(Msg, Dict) ->
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

get_avp_value(_, Name, [_MsgName | Avps]) ->
    case lists:keyfind(Name, 1, Avps) of
        {_, V} ->
            V;
        _ ->
            undefined
    end;

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

-define(ALL_INFO, [capabilities,
                   applications,
                   transport,
                   pending,
                   statistics]).

service_info(Items, S)
  when is_list(Items) ->
    [{complete(I), service_info(I,S)} || I <- Items];
service_info(Item, S)
  when is_atom(Item) ->
    service_info(Item, S, true).

service_info(Item, #state{service = Svc} = S, Complete) ->
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
        pending      -> info_pending(S);
        statistics   -> info_stats(S);
        keys         -> ?ALL_INFO ++ ?CAP_INFO;  %% mostly for test
        all          -> service_info(?ALL_INFO, S);
        _ when Complete   -> service_info(complete(Item), S, false);
        _            -> undefined
    end.

complete(Pre) ->
    P = atom_to_list(Pre),
    case [I || I <- [name | ?ALL_INFO] ++ ?CAP_INFO,
               lists:prefix(P, atom_to_list(I))]
    of
        [I] -> I;
        _   -> Pre
    end.

info_stats(#state{peerT = PeerT}) ->
    Peers = ets:select(PeerT, [{#peer{ref = '$1', conn = '$2', _ = '_'},
                                [{'is_pid', '$2'}],
                                [['$1', '$2']]}]),
    diameter_stats:read(lists:append(Peers)).
%% TODO: include peer identities in return value

info_transport(#state{peerT = PeerT, connT = ConnT}) ->
    dict:fold(fun it/3,
              [],
              ets:foldl(fun(T,A) -> it_acc(ConnT, A, T) end,
                        dict:new(),
                        PeerT)).

it(Ref, [[{type, connect} | _] = L], Acc) ->
    [[{ref, Ref} | L] | Acc];
it(Ref, [[{type, accept}, {options, Opts} | _] | _] = L, Acc) ->
    [[{ref, Ref},
      {type, listen},
      {options, Opts},
      {accept, [lists:nthtail(2,A) || A <- L]}]
     | Acc].
%% Each entry has the same Opts. (TODO)

it_acc(ConnT, Acc, #peer{pid = Pid,
                         type = Type,
                         ref = Ref,
                         options = Opts,
                         op_state = OS,
                         started = T,
                         conn = TPid}) ->
    dict:append(Ref,
                [{type, Type},
                 {options, Opts},
                 {watchdog, {Pid, T, OS}}
                 | info_conn(ConnT, TPid)],
                Acc).

info_conn(ConnT, TPid) ->
    info_conn(ets:lookup(ConnT, TPid)).

info_conn([#conn{pid = Pid, apps = SApps, caps = Caps, started = T}]) ->
    [{peer, {Pid, T}},
     {apps, SApps},
     {caps, info_caps(Caps)}];
info_conn([] = No) ->
    No.

info_caps(#diameter_caps{} = C) ->
    lists:zip(record_info(fields, diameter_caps), tl(tuple_to_list(C))).

info_apps(#state{service = #diameter_service{applications = Apps}}) ->
    lists:map(fun mk_app/1, Apps).

mk_app(#diameter_app{alias = Alias,
                     dictionary = Dict,
                     module = ModX,
                     id = Id}) ->
    [{alias, Alias},
     {dictionary, Dict},
     {module, ModX},
     {id, Id}].

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

    ets:select(?REQUEST_TABLE, MatchSpec).
