%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

-compile({no_auto_import, [monitor/2]}).

-export([start_service/2,
         stop_service/1,
         add_transport/2,
         remove_transport/2,
         have_transport/2,
         lookup/1]).

%% child server start
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% diameter_sync requests.
-export([sync/1]).

%% debug
-export([state/0,
         uptime/0]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% Server state.
-record(state, {id = diameter_lib:now()}).

%% Registered name of the server.
-define(SERVER, ?MODULE).

%% Table config is written to.
-define(TABLE, ?MODULE).

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

%% The default sequence mask.
-define(NOMASK, {0,32}).

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

state() ->
    call(state).

uptime() ->
    call(uptime).

%%% ----------------------------------------------------------
%%% # init/1
%%% ----------------------------------------------------------

init([]) ->
    {ok, #state{}}.

%%% ----------------------------------------------------------
%%% # handle_call/2
%%% ----------------------------------------------------------

handle_call(state, _, State) ->
    {reply, State, State};

handle_call(uptime, _, #state{id = Time} = State) ->
    {reply, diameter_lib:now_diff(Time), State};

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

%% A service process has died. This is most likely a consequence of
%% stop_service, in which case the restart will find no config for the
%% service and do nothing. The entry keyed on the monitor ref is only
%% removed as a result of the 'DOWN' notification however.
handle_info({'DOWN', MRef, process, _, Reason}, State) ->
    [#monitor{service = SvcName} = T] = select([{#monitor{mref = MRef,
                                                          _ = '_'},
                                                 [],
                                                 ['$_']}]),
    queue_restart(Reason, SvcName),
    delete_object(T),
    {noreply, State};

handle_info({monitor, SvcName, Pid}, State) ->
    monitor(Pid, SvcName),
    {noreply, State};

handle_info({restart, SvcName}, State) ->
    restart(SvcName),
    {noreply, State};

handle_info(restart, State) ->
    restart(),
    {noreply, State};

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

sync({add, SvcName, Type, Opts}) ->
    try
        add(SvcName, Type, Opts)
    catch
        ?FAILURE(Reason) -> {error, Reason}
    end;

sync({remove, SvcName, Pred}) ->
    remove(select([{#transport{service = '$1', _ = '_'},
                    [{'=:=', '$1', {const, SvcName}}],
                    ['$_']}]),
           SvcName,
           Pred).

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

monitor(Pid, SvcName) ->
    MRef = erlang:monitor(process, Pid),
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

add(SvcName, Type, Opts) ->
    %% Ensure acceptable transport options. This won't catch all
    %% possible errors (faulty callbacks for example) but it catches
    %% many. diameter_service:merge_service/2 depends on usable
    %% capabilities for example.
    ok = transport_opts(Opts),

    Ref = make_ref(),
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
    lists:foreach(fun(T) -> opt(T) orelse ?THROW({invalid, T}) end, Opts).

opt({transport_module, M}) ->
    is_atom(M);

opt({transport_config, _, Tmo}) ->
    ?IS_UINT32(Tmo) orelse Tmo == infinity;

opt({applications, As}) ->
    is_list(As);

opt({capabilities, Os}) ->
    is_list(Os) andalso ok == encode_CER(Os);

opt({K, Tmo})
  when K == capx_timeout;
       K == dpr_timeout;
       K == dpa_timeout ->
    ?IS_UINT32(Tmo);

opt({length_errors, T}) ->
    lists:member(T, [exit, handle, discard]);

opt({K, Tmo})
  when K == reconnect_timer;  %% deprecated
       K == connect_timer ->
    ?IS_UINT32(Tmo);

opt({watchdog_timer, {M,F,A}})
  when is_atom(M), is_atom(F), is_list(A) ->
    true;
opt({watchdog_timer, Tmo}) ->
    ?IS_UINT32(Tmo);

opt({watchdog_config, L}) ->
    is_list(L) andalso lists:all(fun wdopt/1, L);

opt({spawn_opt, Opts}) ->
    is_list(Opts);

opt({pool_size, N}) ->
    is_integer(N) andalso 0 < N;

%% Options that we can't validate.
opt({K, _})
  when K == transport_config;
       K == capabilities_cb;
       K == disconnect_cb;
       K == private ->
    true;

%% Anything else, which is ignored by us. This makes options sensitive
%% to spelling mistakes but arbitrary options are passed by some users
%% as a way to identify transports. (That is, can't just do away with
%% it.)
opt(_) ->
    true.

wdopt({K,N}) ->
    (K == okay orelse K == suspect) andalso is_integer(N) andalso 0 =< N;
wdopt(_) ->
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

%% remove/3

remove(L, SvcName, Pred) ->
    rm(SvcName, lists:filter(fun(#transport{ref = R, type = T, options = O}) ->
                                     Pred(R,T,O)
                             end,
                             L)).

rm(_, []) ->
    ok;
rm(SvcName, L) ->
    Refs = lists:map(fun(#transport{ref = R}) -> R end, L),
    case stop_transport(SvcName, Refs) of
        ok ->
            diameter_stats:flush(Refs),
            lists:foreach(fun delete_object/1, L);
        {error, _} = No ->
            No
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
    Apps = init_apps(AppOpts),

    [] == Apps andalso ?THROW(no_apps),

    %% Use the fact that diameter_caps has the same field names as CER.
    Fields = ?BASE:'#info-'(diameter_base_CER) -- ['AVP'],

    CapOpts = [T || {K,_} = T <- Opts, lists:member(K, Fields)],
    Caps = make_caps(#diameter_caps{}, CapOpts),

    ok = encode_CER(CapOpts),

    SvcOpts = make_opts((Opts -- AppOpts) -- CapOpts,
                        [{false, share_peers},
                         {false, use_shared_peers},
                         {false, monitor},
                         {?NOMASK, sequence},
                         {nodes, restrict_connections},
                         {16#FFFFFF, incoming_maxlen},
                         {true, strict_mbit},
                         {true, string_decode},
                         {[], spawn_opt}]),

    D = proplists:get_value(string_decode, SvcOpts, true),

    #service{name = SvcName,
             rec = #diameter_service{applications = Apps,
                                     capabilities = binary_caps(Caps, D)},
             options = SvcOpts}.

binary_caps(Caps, true) ->
    Caps;
binary_caps(Caps, false) ->
    diameter_capx:binary_caps(Caps).

%% make_opts/2

make_opts(Opts, Defs) ->
    Known = [{K, get_opt(K, Opts, D)} || {D,K} <- Defs],
    Unknown = Opts -- Known,

    [] == Unknown orelse ?THROW({invalid, hd(Unknown)}),

    [{K, opt(K,V)} || {K,V} <- Known].

opt(incoming_maxlen, N)
  when 0 =< N, N < 1 bsl 24 ->
    N;

opt(spawn_opt, L)
  when is_list(L) ->
    L;

opt(K, false = B)
  when K == share_peers;
       K == use_shared_peers;
       K == monitor;
       K == restrict_connections;
       K == strict_mbit;
       K == string_decode ->
    B;

opt(K, true = B)
  when K == share_peers;
       K == use_shared_peers;
       K == strict_mbit;
       K == string_decode ->
    B;

opt(restrict_connections, T)
  when T == node;
       T == nodes ->
    T;

opt(K, T)
  when (K == share_peers
        orelse K == use_shared_peers
        orelse K == restrict_connections), ([] == T
                                            orelse is_atom(hd(T))) ->
    T;

opt(monitor, P)
  when is_pid(P) ->
    P;

opt(K, F)
  when K == restrict_connections;
       K == share_peers;
       K == use_shared_peers ->
    try diameter_lib:eval(F) of  %% but no guarantee that it won't fail later
        Nodes when is_list(Nodes) ->
            F;
        V ->
            ?THROW({value, {K,V}})
    catch
        E:R ->
            ?THROW({value, {K, E, R, ?STACK}})
    end;

opt(sequence, {_,_} = T) ->
    sequence(T);

opt(sequence = K, F) ->
    try diameter_lib:eval(F) of
        T -> sequence(T)
    catch
        E:R ->
            ?THROW({value, {K, E, R, ?STACK}})
    end;

opt(K, _) ->
    ?THROW({value, K}).

sequence({H,N} = T)
  when 0 =< N, N =< 32, 0 =< H, 0 == H bsr (32-N) ->
    T;

sequence(_) ->
    ?THROW({value, sequence}).

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

init_apps(Opts) ->
    lists:foldl(fun app_acc/2, [], lists:reverse(Opts)).

app_acc({application, Opts} = T, Acc) ->
    is_list(Opts) orelse ?THROW(T),

    [Dict, Mod] = get_opt([dictionary, module], Opts),
    Alias = get_opt(alias, Opts, Dict),
    ModS  = get_opt(state, Opts, Alias),
    M = get_opt(call_mutates_state, Opts, false, [true]),
    A = get_opt(answer_errors, Opts, discard, [callback, report]),
    P = get_opt(request_errors, Opts, answer_3xxx, [answer, callback]),
    [#diameter_app{alias = Alias,
                   dictionary = Dict,
                   id = cb(Dict, id),
                   module = init_mod(Mod),
                   init_state = ModS,
                   mutable = M,
                   options = [{answer_errors, A},
                              {request_errors, P}]}
     | Acc].

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

%% Retreive and validate.
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
        E: Reason ->
            ?THROW({callback, E, Reason, ?STACK})
    end.

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
