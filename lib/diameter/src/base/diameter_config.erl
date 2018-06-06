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
%% This module writes service/transport configuration to the table
%% diameter_config, so that the config will survive service process
%% death, and then turns it into calls towards diameter_service. It
%% also restarts services upon their death.
%%
%% The table diameter_config is only written here while
%% diameter_service reads. This is all somewhat after the fact. Once
%% upon a time the config was only stored in the service process,
%% causing much grief if these processes died (which they did with
%% some regularity) and one was forced to reconfigure. This module was
%% then inserted into the service start in order to keep a more
%% permanent record of the config. That said, service processes are
%% now much more robust than they once were and crashing is a thing of
%% the past.
%%

-module(diameter_config).
-behaviour(gen_server).

-export([start_service/2,
         stop_service/1,
         add_transport/2,
         remove_transport/2,
         have_transport/2,
         lookup/1,
         subscribe/2]).

%% server start
-export([start_link/0,
         start_link/1]).

%% gen_server callbacks
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% callbacks
-export([sync/1]).    %% diameter_sync requests

%% debug
-export([state/0,
         uptime/0]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% Server state.
-record(state, {id = diameter_lib:now(),
                role :: server | transport}).

%% Registered name of the server.
-define(SERVER, ?MODULE).

%% Table config is written to.
-define(TABLE, ?MODULE).

%% Key on which a transport-specific child registers itself.
-define(TRANSPORT_KEY(Ref), {?MODULE, transport, Ref}).

%% Workaround for dialyzer's lack of understanding of match specs.
-type match(T)
   :: T | '_' | '$1' | '$2' | '$3' | '$4'.

%% Configuration records in ?TABLE.

-record(service,
        {name,
         rec     :: match(#diameter_service{}),
         options :: match(list())}).

-record(transport,
        {service, %% name
         ref = make_ref() :: match(reference()),
         type             :: match(connect | listen),
         options          :: match(list())}).

%% Monitor entry in ?TABLE.
-record(monitor, {mref = make_ref() :: reference(),
                  service}). %% name

%% Time to lay low before restarting a dead service.
-define(RESTART_SLEEP, 2000).

%% Test for a valid timeout.
-define(IS_UINT32(N),
        is_integer(N) andalso 0 =< N andalso 0 == N bsr 32).

%% A minimal diameter_caps for checking for valid capabilities values.
-define(EXAMPLE_CAPS,
        #diameter_caps{origin_host = "TheHost",
                       origin_realm = "TheRealm",
                       host_ip_address = [{127,0,0,1}],
                       vendor_id = 42,
                       product_name = "TheProduct"}).

-define(VALUES(Rec), tl(tuple_to_list(Rec))).

%% The RFC 3588 common dictionary is used to validate capabilities
%% configuration. That a given transport may use the RFC 6733
%% dictionary is of no consequence.
-define(BASE, diameter_gen_base_rfc3588).

%%% The return values below assume the server diameter_config is started.
%%% The functions will exit if it isn't.

%% --------------------------------------------------------------------------
%% # start_service/2
%% --------------------------------------------------------------------------

-spec start_service(diameter:service_name(), [diameter:service_opt()])
   -> ok
    | {error, term()}.

start_service(SvcName, Opts)
  when is_list(Opts)  ->
    start_rc(sync(SvcName, {start_service, SvcName, Opts})).

start_rc({ok = T, _Pid}) ->
    T;
start_rc({error, _} = No) ->
    No;
start_rc(timeout) ->
    {error, application_not_started}.

%% --------------------------------------------------------------------------
%% # stop_service/1
%% --------------------------------------------------------------------------

-spec stop_service(diameter:service_name())
   -> ok.

stop_service(SvcName) ->
    sync(SvcName, {stop_service, SvcName}).

%% --------------------------------------------------------------------------
%% # add_transport/2
%% --------------------------------------------------------------------------

-spec add_transport(diameter:service_name(),
                    {connect|listen, [diameter:transport_opt()]})
   -> {ok, diameter:transport_ref()}
    | {error, term()}.

add_transport(SvcName, {T, Opts})
  when is_list(Opts), (T == connect orelse T == listen) ->
    sync(SvcName, {add, SvcName, T, Opts}).

%% --------------------------------------------------------------------------
%% # remove_transport(SvcName, Pred)
%%
%% Input:  Pred = arity 3 fun on transport ref, connect|listen and Opts,
%%                returning true if the transport is to be removed, false if
%%                not
%%              | arity 2 fun on Ref and Opts only
%%              | arity 1 fun on Opts only
%%              | Opts matching all transports that have all of the specified
%%                options
%%              | Ref matching only the transport with this reference.
%%              | {M,F,A} applied to Ref, connect|listen and Opts
%%              | boolean()
%%
%% Output: ok | {error, Reason}
%% --------------------------------------------------------------------------

-spec remove_transport(diameter:service_name(), diameter:transport_pred())
   -> ok
    | {error, term()}.

remove_transport(SvcName, Pred) ->
    try
        sync(SvcName, {remove, SvcName, pred(Pred)})
    catch
        ?FAILURE(Reason) ->
            {error, Reason}
    end.

pred(Pred)
  when is_function(Pred, 3) ->
    Pred;
pred(Pred)
  when is_function(Pred, 2) ->
    fun(R,_,O) -> Pred(R,O) end;
pred(Pred)
  when is_function(Pred, 1) ->
    fun(_,_,O) -> Pred(O) end;
pred(Opts)
  when is_list(Opts) ->
    fun(_,_,O) -> [] == Opts -- O end;
pred(Ref)
  when is_reference(Ref) ->
    fun(R,_,_) -> R == Ref end;
pred({M,F,A})
  when is_atom(M), is_atom(F), is_list(A) ->
    fun(R,T,O) -> apply(M,F,[R,T,O|A]) end;
pred({Type, Pred}) ->  %% backwards compatibility
    P = pred(Pred),
    fun(R,T,O) -> T == Type andalso P(R,T,O) end;
pred(B)
  when is_boolean(B) ->
    fun(_,_,_) -> B end;
pred(_) ->
    ?THROW(pred).

%% --------------------------------------------------------------------------
%% # subscribe/2
%% --------------------------------------------------------------------------

subscribe(Ref, T) ->
    diameter_reg:subscribe(?TRANSPORT_KEY(Ref), T).

%% --------------------------------------------------------------------------
%% # have_transport/2
%%
%% Output: true | false
%% --------------------------------------------------------------------------

have_transport(SvcName, Ref) ->
    member([{#transport{service = '$1',
                        ref = '$2',
                        _ = '_'},
             [{'andalso', {'=:=', '$1', {const, SvcName}},
                          {'=:=', '$2', {const, Ref}}}],
             [true]}]).

%% --------------------------------------------------------------------------
%% # lookup/1
%% --------------------------------------------------------------------------

lookup(SvcName) ->
    select([{#service{name = '$1', rec = '$2', options = '$3'},
             [{'=:=', '$1', {const, SvcName}}],
             [{{'$1', '$2', '$3'}}]},
            {#transport{service = '$1',
                        ref = '$2',
                        type = '$3',
                        options = '$4'},
             [{'=:=', '$1', {const, SvcName}}],
             [{{'$2', '$3', '$4'}}]}]).

%% ---------------------------------------------------------
%% EXPORTED INTERNAL FUNCTIONS
%% ---------------------------------------------------------

start_link() ->
    ServerName = {local, ?SERVER},
    Module     = ?MODULE,
    Args       = [],
    Options    = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(ServerName, Module, Args, Options).

start_link(T) ->
    proc_lib:start_link(?MODULE, init, [T], infinity, []).

state() ->
    call(state).

uptime() ->
    call(uptime).

%%% ----------------------------------------------------------
%%% # init/1
%%% ----------------------------------------------------------

%% ?SERVER start.
init([]) ->
    {ok, #state{role = server}};

%% Child start as a consequence of add_transport.
init({SvcName, Type, Opts}) ->
    Res = try
              add(SvcName, Type, Opts)
          catch
              ?FAILURE(Reason) -> {error, Reason}
          end,
    proc_lib:init_ack({ok, self(), Res}),
    loop(Res).

%% loop/1

loop({ok, _}) ->
    gen_server:enter_loop(?MODULE, [], #state{role = transport});

loop({error, _}) ->
    ok.  %% die

%%% ----------------------------------------------------------
%%% # handle_call/2
%%% ----------------------------------------------------------

handle_call(state, _, State) ->
    {reply, State, State};

handle_call(uptime, _, #state{id = Time} = S) ->
    {reply, diameter_lib:now_diff(Time), S};

handle_call(Req, From, State) ->
    ?UNEXPECTED([Req, From]),
    Reply = {error, {bad_request, Req}},
    {reply, Reply, State}.

%%% ----------------------------------------------------------
%%% # handle_cast/2
%%% ----------------------------------------------------------

handle_cast(Msg, State) ->
    ?UNEXPECTED([Msg]),
    {noreply, State}.

%%% ----------------------------------------------------------
%%% # handle_info/2
%%% ----------------------------------------------------------

%% remove_transport is telling published child to die.
handle_info(stop, #state{role = transport} = S) ->
    {stop, normal, S};

%% A service process has died. This is most likely a consequence of
%% stop_service, in which case the restart will find no config for the
%% service and do nothing. The entry keyed on the monitor ref is only
%% removed as a result of the 'DOWN' notification however.
handle_info({'DOWN', MRef, process, _, Reason}, #state{role = server} = S) ->
    [#monitor{service = SvcName} = T] = select([{#monitor{mref = MRef,
                                                          _ = '_'},
                                                 [],
                                                 ['$_']}]),
    queue_restart(Reason, SvcName),
    delete_object(T),
    {noreply, S};

handle_info({monitor, SvcName, Pid}, #state{role = server} = S) ->
    insert_monitor(Pid, SvcName),
    {noreply, S};

handle_info({restart, SvcName}, #state{role = server} = S) ->
    restart(SvcName),
    {noreply, S};

handle_info(restart, #state{role = server} = S) ->
    restart(),
    {noreply, S};

handle_info(Info, State) ->
    ?UNEXPECTED([Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% # terminate/2
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%% ----------------------------------------------------------
%%% # code_change/3
%%% ----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ---------------------------------------------------------

insert(T) ->
    ets:insert(?TABLE, T).

%% ?TABLE is a bag: check only for a service entry.
have_service(SvcName) ->
    member([{#service{name = '$1', _ = '_'},
             [{'=:=', '$1', {const, SvcName}}],
             [true]}]).

member(MatchSpec) ->
    '$end_of_table' =/= ets:select(?TABLE, MatchSpec, 1).

delete_object(T) ->
    ets:delete_object(?TABLE, T).

delete(Key) ->
    ets:delete(?TABLE, Key).

select(MatchSpec) ->
    ets:select(?TABLE, MatchSpec).

select_delete(MatchSpec) ->
    ets:select_delete(?TABLE, MatchSpec).

%% sync/2
%%
%% Interface functions used to be implemented as calls to ?SERVER but
%% now serialize things per service instead since stopping a service
%% can take time if the server doesn't answer DPR. A caller who wants
%% to stop multiple services can then improve performance by spawning
%% processes to stop them concurrently.

sync(SvcName, T) ->
    diameter_sync:call({?MODULE, SvcName},
		       {?MODULE, sync, [T]},
		       infinity,
		       infinity).

%% sync/1

sync({restart, SvcName}) ->
    have_service(SvcName) andalso start(SvcName);

sync({start_service, SvcName, Opts}) ->
    try
        start(have_service(SvcName), SvcName, Opts)
    catch
        ?FAILURE(Reason) -> {error, Reason}
    end;

sync({stop_service, SvcName}) ->
    stop(SvcName);

%% Start a child whose only purpose is to be alive for the lifetime of
%% the transport configuration and publish itself in diameter_reg.
%% This is to provide a way for processes to to be notified when the
%% configuration is removed (diameter_reg:subscribe/2).
sync({add, SvcName, Type, Opts}) ->
    {ok, _Pid, Res} = diameter_config_sup:start_child({SvcName, Type, Opts}),
    Res;

sync({remove, SvcName, Pred}) ->
    Recs = select([{#transport{service = '$1', _ = '_'},
                    [{'=:=', '$1', {const, SvcName}}],
                    ['$_']}]),
    F = fun(#transport{ref = R, type = T, options = O}) ->
                Pred(R,T,O)
        end,
    remove(SvcName, lists:filter(F, Recs)).

%% start/3

start(true, _, _) ->
    {error, already_started};
start(false, SvcName, Opts) ->
    insert(make_config(SvcName, Opts)),
    start(SvcName).

%% start/1

start(SvcName) ->
    RC = diameter_service:start(SvcName),
    startmon(SvcName, RC),
    RC.

startmon(SvcName, {ok, Pid}) ->
    ?SERVER ! {monitor, SvcName, Pid};
startmon(_, {error, _}) ->
    ok.

insert_monitor(Pid, SvcName) ->
    MRef = monitor(process, Pid),
    insert(#monitor{mref = MRef, service = SvcName}).

%% queue_restart/2

%% Service has gone down on monitor death. Note that all service-related
%% config is deleted.
queue_restart({shutdown, {monitor, _}}, SvcName) ->
    delete(SvcName);

%% Application shutdown: ignore.
queue_restart(shutdown, _) ->
    ok;

%% Or not.
queue_restart(_, SvcName) ->
    erlang:send_after(?RESTART_SLEEP, self(), {restart, SvcName}).

%% restart/1

restart(SvcName) ->
    sync(SvcName, {restart, SvcName}).

%% restart/0
%%
%% Start anything configured as required. Bang 'restart' to the server
%% to kick things into gear manually. (Not that it should be required
%% but it's been useful for test.)

restart() ->
    MatchSpec = [{#service{name = '$1', _ = '_'},
                  [],
                  ['$1']}],
    lists:foreach(fun restart/1, select(MatchSpec)).

%% stop/1

stop(SvcName) ->
    %% If the call to the service returns error for any reason other
    %% than the process not being alive then deleting the config from
    %% under it will surely bring it down.
    diameter_service:stop(SvcName),
    %% Delete only the service entry, not everything keyed on the name,
    select_delete([{#service{name = '$1', _ = '_'},
                    [{'=:=', '$1', {const, SvcName}}],
                    [true]}]),
    ok.
%% Note that a transport has to be removed for its statistics to be
%% deleted.

%% add/3

%% Can't check for a single common dictionary since a transport may
%% restrict applications so that that there's one while the service
%% has many.

add(SvcName, Type, Opts0) ->
    %% Ensure acceptable transport options. This won't catch all
    %% possible errors (faulty callbacks for example) but it catches
    %% many. diameter_service:merge_service/2 depends on usable
    %% capabilities for example.
    Opts = transport_opts(Opts0),

    Ref = make_ref(),
    true = diameter_reg:add_new(?TRANSPORT_KEY(Ref)),
    T = {Ref, Type, Opts},
    %% The call to the service returns error if the service isn't
    %% started yet, which is harmless. The transport will be started
    %% when the service is in that case.
    case start_transport(SvcName, T) of
        ok ->
            insert(#transport{service = SvcName,
                              ref = Ref,
                              type = Type,
                              options = Opts}),
            {ok, Ref};
        {error, _} = No ->
            No
    end.

transport_opts(Opts) ->
    [setopt(transport, T) || T <- Opts].

%% setopt/2

setopt(K, T) ->
    case opt(K, T) of
        {value, X} ->
            X;
        true ->
            T;
        false ->
            ?THROW({invalid, T});
        {error, Reason} ->
            ?THROW({invalid, T, Reason})
    end.

%% opt/2

opt(_, {incoming_maxlen, N}) ->
    is_integer(N) andalso 0 =< N andalso N < 1 bsl 24;

opt(service, {K, B})
  when K == string_decode;
       K == traffic_counters ->
    is_boolean(B);

opt(service, {K, false})
  when K == share_peers;
       K == use_shared_peers;
       K == monitor;
       K == restrict_connections;
       K == strict_arities ->
    true;

opt(service, {K, true})
  when K == share_peers;
       K == use_shared_peers;
       K == strict_arities ->
    true;

opt(service, {decode_format, T})
  when T == record;
       T == list;
       T == map;
       T == none;
       T == record_from_map ->
    true;

opt(service, {strict_arities, T})
  when T == encode;
       T == decode ->
    true;

opt(service, {restrict_connections, T})
  when T == node;
       T == nodes ->
    true;

opt(service, {K, T})
  when (K == share_peers
        orelse K == use_shared_peers
        orelse K == restrict_connections), ([] == T
                                            orelse is_atom(hd(T))) ->
    true;

opt(service, {monitor, P}) ->
    is_pid(P);

opt(service, {K, F})
  when K == restrict_connections;
       K == share_peers;
       K == use_shared_peers ->
    try diameter_lib:eval(F) of  %% but no guarantee that it won't fail later
        Nodes ->
            is_list(Nodes) orelse {error, Nodes}
    catch
        E:R:Stack ->
            {error, {E, R, Stack}}
    end;

opt(service, {sequence, {H,N}}) ->
    0 =< N andalso N =< 32
        andalso is_integer(H)
        andalso 0 =< H
        andalso 0 == H bsr (32-N);

opt(service = S, {sequence = K, F}) ->
    try diameter_lib:eval(F) of
        {_,_} = T ->
            KT = {K,T},
            opt(S, KT) andalso {value, KT};
        V ->
            {error, V}
    catch
        E:R:Stack ->
            {error, {E, R, Stack}}
    end;

opt(transport, {transport_module, M}) ->
    is_atom(M);

opt(transport, {transport_config, _, Tmo}) ->
    ?IS_UINT32(Tmo) orelse Tmo == infinity;

opt(transport, {applications, As}) ->
    is_list(As);

opt(transport, {capabilities, Os}) ->
    is_list(Os) andalso try ok = encode_CER(Os), true
                        catch ?FAILURE(No) -> {error, No}
                        end;

opt(_, {K, Tmo})
  when K == capx_timeout;
       K == dpr_timeout;
       K == dpa_timeout ->
    ?IS_UINT32(Tmo);

opt(_, {capx_strictness, B}) ->
    is_boolean(B) andalso {value, {strict_capx, B}};
opt(_, {K, B})
  when K == strict_capx;
       K == strict_mbit ->
    is_boolean(B);

opt(_, {avp_dictionaries, Mods}) ->
    is_list(Mods) andalso lists:all(fun erlang:is_atom/1, Mods);

opt(_, {length_errors, T}) ->
    lists:member(T, [exit, handle, discard]);

opt(transport, {reconnect_timer, Tmo}) ->  %% deprecated
    ?IS_UINT32(Tmo) andalso {value, {connect_timer, Tmo}};
opt(_, {connect_timer, Tmo}) ->
    ?IS_UINT32(Tmo);

opt(_, {watchdog_timer, {M,F,A}})
  when is_atom(M), is_atom(F), is_list(A) ->
    true;
opt(_, {watchdog_timer, Tmo}) ->
    ?IS_UINT32(Tmo);

opt(_, {watchdog_config, L}) ->
    is_list(L) andalso lists:all(fun wd/1, L);

opt(_, {spawn_opt, {M,F,A}})
  when is_atom(M), is_atom(F), is_list(A) ->
    true;
opt(_, {spawn_opt = K, Opts}) ->
    if is_list(Opts) ->
            {value, {K, spawn_opts(Opts)}};
       true ->
            false
    end;

opt(_, {pool_size, N}) ->
    is_integer(N) andalso 0 < N;

%% Options we can't validate.
opt(_, {K, _})
  when K == disconnect_cb;
       K == capabilities_cb ->
    true;
opt(transport, {K, _})
  when K == transport_config;
       K == private ->
    true;

%% Anything else, which is ignored in transport config. This makes
%% options sensitive to spelling mistakes, but arbitrary options are
%% passed by some users as a way to identify transports so can't just
%% do away with it.
opt(K, _) ->
    K == transport.

%% wd/1

wd({K,N}) ->
    (K == okay orelse K == suspect) andalso is_integer(N) andalso 0 =< N;
wd(_) ->
    false.

%% start_transport/2

start_transport(SvcName, T) ->
    case diameter_service:start_transport(SvcName, T) of
        {ok, _Pid} ->
            ok;
        {error, no_service} ->
            ok;
        {error, _} = No ->
            No
    end.

%% remove/2

remove(_, []) ->
    ok;

remove(SvcName, L) ->
    Refs = lists:map(fun(#transport{ref = R}) -> R end, L),
    case stop_transport(SvcName, Refs) of
        ok ->
            lists:foreach(fun stop_child/1, Refs),
            diameter_stats:flush(Refs),
            lists:foreach(fun delete_object/1, L);
        {error, _} = No ->
            No
    end.

stop_child(Ref) ->
    case diameter_reg:match(?TRANSPORT_KEY(Ref)) of
        [{_, Pid}] ->  %% tell the transport-specific child to die
            Pid ! stop;
        [] ->          %% already removed/dead
            ok
    end.

stop_transport(SvcName, Refs) ->
    case diameter_service:stop_transport(SvcName, Refs) of
        ok ->
            ok;
        {error, no_service} ->
            ok;
        {error, _} = No ->
            No
    end.

%% make_config/2

make_config(SvcName, Opts) ->
    AppOpts = [T || {application, _} = T <- Opts],
    Apps = [init_app(T) || T <- AppOpts],

    [] == Apps andalso ?THROW(no_apps),

    %% Use the fact that diameter_caps has the same field names as CER.
    Fields = ?BASE:'#info-'(diameter_base_CER) -- ['AVP'],

    CapOpts = [T || {K,_} = T <- Opts, lists:member(K, Fields)],
    Caps = make_caps(#diameter_caps{}, CapOpts),

    ok = encode_CER(CapOpts),

    SvcOpts = service_opts((Opts -- AppOpts) -- CapOpts),

    D = proplists:get_value(string_decode, SvcOpts, true),

    #service{name = SvcName,
             rec = #diameter_service{applications = Apps,
                                     capabilities = binary_caps(Caps, D)},
             options = SvcOpts}.

binary_caps(Caps, true) ->
    Caps;
binary_caps(Caps, false) ->
    diameter_capx:binary_caps(Caps).

%% service_opts/1

service_opts(Opts) ->
    Res = [setopt(service, T) || T <- Opts],
    Keys = sets:to_list(sets:from_list([K || {K,_} <- Res])), %% unique
    Dups = lists:foldl(fun(K,A) -> lists:keydelete(K, 1, A) end, Res, Keys),
    [] == Dups orelse ?THROW({duplicate, Dups}),
    Res.
%% Reject duplicates on a service, but not on a transport. There's no
%% particular reason for the inconsistency, but the historic behaviour
%% ignores all but the first of a transport_opt(), and there's no real
%% reason to change it.

spawn_opts(L) ->
    [T || T <- L, T /= link, T /= monitor].

make_caps(Caps, Opts) ->
    case diameter_capx:make_caps(Caps, Opts) of
        {ok, T} ->
            T;
        {error, Reason} ->
            ?THROW(Reason)
    end.

%% Validate types by encoding a CER.
encode_CER(Opts) ->
    {ok, CER} = diameter_capx:build_CER(make_caps(?EXAMPLE_CAPS, Opts),
                                        ?BASE),

    Hdr = #diameter_header{version = ?DIAMETER_VERSION,
                           end_to_end_id = 0,
                           hop_by_hop_id = 0},

    try
        diameter_codec:encode(?BASE, #diameter_packet{header = Hdr,
                                                      msg = CER}),
        ok
    catch
        exit: Reason ->
            ?THROW(Reason)
    end.

init_app({application, Opts} = T) ->
    is_list(Opts) orelse ?THROW(T),

    [Dict, Mod] = get_opt([dictionary, module], Opts),
    Alias = get_opt(alias, Opts, Dict),
    ModS  = get_opt(state, Opts, Alias),
    M = get_opt(call_mutates_state, Opts, false, [true]),
    A = get_opt(answer_errors, Opts, discard, [callback, report]),
    P = get_opt(request_errors, Opts, answer_3xxx, [answer, callback]),
    #diameter_app{alias = Alias,
                  dictionary = Dict,
                  id = cb(Dict, id),
                  module = init_mod(Mod),
                  init_state = ModS,
                  mutable = M,
                  options = [{answer_errors, A},
                             {request_errors, P}]}.

init_mod(#diameter_callback{} = R) ->
    init_mod([diameter_callback, R]);
init_mod([diameter_callback, #diameter_callback{}] = L) ->
    L;
init_mod([diameter_callback = M | L])
  when is_list(L) ->
    [M, init_cb(L)];
init_mod(M)
  when is_atom(M) ->
    [M];
init_mod([M|_] = L)
  when is_atom(M) ->
    L;
init_mod(M) ->
    ?THROW({module, M}).

init_cb(List) ->
    Fields = record_info(fields, diameter_callback),
    Defaults = lists:zip(Fields, tl(tuple_to_list(#diameter_callback{}))),
    Values = [V || F <- Fields,
                   D <- [proplists:get_value(F, Defaults)],
                   V <- [proplists:get_value(F, List, D)]],
    #diameter_callback{} = list_to_tuple([diameter_callback | Values]).

%% Retrieve and validate.
get_opt(Key, List, Def, Other) ->
    init_opt(Key, get_opt(Key, List, Def), [Def|Other]).

init_opt(_, V, [V|_]) ->
    V;
init_opt(Name, V, [_|Vals]) ->
    init_opt(Name, V, Vals);
init_opt(Name, V, []) ->
    ?THROW({Name, V}).

%% Get a single value at the specified key.
get_opt(Keys, List)
  when is_list(Keys) ->
    [get_opt(K, List) || K <- Keys];
get_opt(Key, List) ->
    case [V || {K,V} <- List, K == Key] of
        [V] -> V;
        _   -> ?THROW({arity, Key})
    end.

%% Get an optional value at the specified key.
get_opt(Key, List, Def) ->
    case [V || {K,V} <- List, K == Key] of
        []  -> Def;
        [V] -> V;
        _   -> ?THROW({arity, Key})
    end.

cb(M,F) ->
    try M:F() of
        V -> V
    catch
        E: Reason: Stack ->
            ?THROW({callback, E, Reason, Stack})
    end.

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
