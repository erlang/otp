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
-compile({no_auto_import, [monitor/2]}).

-behaviour(gen_server).

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
-record(state, {id = now()}).

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

%% Time to lay low before restarting a dead service.
-define(RESTART_SLEEP, 2000).

%% A minimal diameter_caps for checking for valid capabilities values.
-define(EXAMPLE_CAPS,
        #diameter_caps{origin_host = "TheHost",
                       origin_realm = "TheRealm",
                       host_ip_address = [{127,0,0,1}],
                       vendor_id = 42,
                       product_name = "TheProduct"}).

-define(VALUES(Rec), tl(tuple_to_list(Rec))).

%%% The return values below assume the server diameter_config is started.
%%% The functions will exit if it isn't.

%% --------------------------------------------------------------------------
%% # start_service(SvcName, Opts)
%%
%% Output: ok | {error, Reason}
%% --------------------------------------------------------------------------

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
%% # stop_service(SvcName)
%%
%% Output: ok
%% --------------------------------------------------------------------------

stop_service(SvcName) ->
    sync(SvcName, {stop_service, SvcName}).

%% --------------------------------------------------------------------------
%% # add_transport(SvcName, {Type, Opts})
%%
%% Input:  Type = connect | listen
%%
%% Output: {ok, Ref} | {error, Reason}
%% --------------------------------------------------------------------------

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

add(SvcName, Type, Opts) ->
    %% Ensure usable capabilities. diameter_service:merge_service/2
    %% depends on this.
    lists:foreach(fun(Os) ->
                          is_list(Os) orelse ?THROW({capabilities, Os}),
                          ok = encode_CER(Os)
                  end,
                  [Os || {capabilities, Os} <- Opts, is_list(Os)]),

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
    Apps = init_apps(Opts),
    [] == Apps andalso ?THROW(no_apps),

    %% Use the fact that diameter_caps has the same field names as CER.
    Fields = diameter_gen_base_rfc3588:'#info-'(diameter_base_CER) -- ['AVP'],

    COpts = [T || {K,_} = T <- Opts, lists:member(K, Fields)],
    Caps = make_caps(#diameter_caps{}, COpts),

    ok = encode_CER(COpts),

    Os = split(Opts, [{[fun erlang:is_boolean/1], false, share_peers},
                      {[fun erlang:is_boolean/1], false, use_shared_peers},
                      {[fun erlang:is_pid/1, false], false, monitor}]),
    %% share_peers and use_shared_peers are currently undocumented.

    #service{name = SvcName,
             rec = #diameter_service{applications = Apps,
                                     capabilities = Caps},
             options = Os}.

make_caps(Caps, Opts) ->
    case diameter_capx:make_caps(Caps, Opts) of
        {ok, T} ->
            T;
        {error, {Reason, _}} ->
            ?THROW(Reason)
    end.

%% Validate types by encoding a CER.
encode_CER(Opts) ->
    {ok, CER} = diameter_capx:build_CER(make_caps(?EXAMPLE_CAPS, Opts)),

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

app_acc({application, Opts}, Acc) ->
    is_list(Opts) orelse ?THROW({application, Opts}),

    [Dict, Mod] = get_opt([dictionary, module], Opts),
    Alias = get_opt(alias, Opts, Dict),
    ModS  = get_opt(state, Opts, Alias),
    M = get_opt(call_mutates_state, Opts, false),
    A = get_opt(answer_errors, Opts, report),
    [#diameter_app{alias = Alias,
                   dictionary = Dict,
                   id = cb(Dict, id),
                   module = init_mod(Mod),
                   init_state = ModS,
                   mutable = init_mutable(M),
                   answer_errors = init_answers(A)}
     | Acc];
app_acc(_, Acc) ->
    Acc.

init_mod(M)
  when is_atom(M) ->
    [M];
init_mod([M|_] = L)
  when is_atom(M) ->
    L;
init_mod(M) ->
    ?THROW({module, M}).

init_mutable(M)
  when M == true;
       M == false ->
    M;
init_mutable(M) ->
    ?THROW({call_mutates_state, M}).

init_answers(A)
  when callback == A;
       report == A;
       discard == A ->
    A;
init_answers(A) ->
    ?THROW({answer_errors, A}).

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

split(Opts, Defs) ->
    [{K, value(D, Opts)} || {_,_,K} = D <- Defs].

value({Preds, Def, Key}, Opts) ->
    V = get_opt(Key, Opts, Def),
    lists:any(fun(P) -> pred(P,V) end, Preds)
        orelse ?THROW({value, Key}),
    V.

pred(F, V)
  when is_function(F) ->
    F(V);
pred(T, V) ->
    T == V.

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
