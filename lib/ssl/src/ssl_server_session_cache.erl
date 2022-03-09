%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2022. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Handle server side pre TLS-1.3 reuse session storage.
%% This implements the RFC session reuse and not the session ticket extension
%% that inspired the RFC session tickets of TLS-1.3.
%%----------------------------------------------------------------------

-module(ssl_server_session_cache).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

%% API
-export([start_link/2,
         new_session_id/1,
         register_session/2,
         reuse_session/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
         %%code_change/3,
         %%format_status/2
        ]).

-record(state, {store_cb,
                lifetime,
                db,
                max,
                session_order,
                id_generator,
                listener
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid(), map()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(ssl_unknown_listener = Listener, Map) ->
    gen_server:start_link({local, Listener}, ?MODULE, [Listener, Map], []);
start_link(Listener, Map) ->
    gen_server:start_link(?MODULE, [Listener, Map], []).

%%--------------------------------------------------------------------
-spec new_session_id(Pid::pid()) -> ssl:session_id().
%%
%% Description: Creates a session id for the server.
%%--------------------------------------------------------------------
new_session_id(Pid) ->
    case call(Pid, new_session_id) of
        {no_server, _} ->
            crypto:strong_rand_bytes(32);
        Result ->
            Result
    end.

%%--------------------------------------------------------------------
-spec reuse_session(pid(), ssl:session_id()) ->  #session{} | not_reusable.
%%
%% Description: Returns session to reuse
%%--------------------------------------------------------------------
reuse_session(Pid, SessionId) ->
    case call(Pid, {reuse_session, SessionId}) of
        {no_server, _} ->
            not_reusable;
        Result ->
            Result
    end.
%%--------------------------------------------------------------------
-spec register_session(pid(), term()) -> ok.
%%
%% Description: Makes a session available for reuse
%%--------------------------------------------------------------------
register_session(Pid, Session) ->
    gen_server:cast(Pid, {register_session, Session}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init(Args :: term()) -> {ok, State :: term()}.
init([Listener, #{lifetime := Lifetime,
                 session_cb := Cb,
                 session_cb_init_args := InitArgs,
                 max := Max
                }]) ->
    process_flag(trap_exit, true),
    Monitor = monitor_listener(Listener),
    DbRef = init(Cb, [{role, server} | InitArgs]),
    State = #state{store_cb = Cb,
                   lifetime = Lifetime,
                   db = DbRef,
                   max = Max,
                   session_order = gb_trees:empty(),
                   id_generator = crypto:strong_rand_bytes(16),
                   listener = Monitor
                  },
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} .
handle_call(new_session_id, _From, #state{id_generator = IdGen} = State) ->
    SessionId = session_id(IdGen),
    {reply, SessionId, State};
handle_call({reuse_session, SessionId}, _From,  #state{store_cb = Cb,
                                                       db = Store0,
                                                       lifetime = Lifetime,
                                                       session_order = Order0} = State0) ->
    case lookup(Cb, Store0, SessionId) of
        undefined ->
            {reply, not_reusable, State0};
        #session{internal_id = InId} = Session ->
            case ssl_session:valid_session(Session, Lifetime) of
                true ->
                    {reply, Session, State0};
                false ->
                    {Store, Order} = invalidate_session(Cb, Store0, Order0, SessionId, InId),
                    {reply, not_reusable, State0#state{db = Store, session_order = Order}}
            end
    end.

-spec handle_cast(Request :: term(), State :: term()) ->
           {noreply, NewState :: term()}.
handle_cast({register_session, #session{session_id = SessionId, time_stamp = TimeStamp} = Session0},
            #state{store_cb = Cb,
                   db = Store0,
                   max = Max,
                   lifetime = Lifetime,
                   session_order = Order0}
            = State0) ->
    InternalId = {TimeStamp, erlang:unique_integer([monotonic])},
    Session = Session0#session{internal_id = InternalId},
    State = case size(Cb, Store0) of
                Max ->
                    %% Throw away oldest session table may not grow larger than max
                    {_, OldSessId, Order1} = gb_trees:take_smallest(Order0),
                    Store1 = delete(Cb, Store0, OldSessId),
                    %% Insert new session
                    Order = gb_trees:insert(InternalId, SessionId, Order1),
                    Store = update(Cb, Store1, SessionId, Session),
                    State0#state{db = Store, session_order = Order};
                Size when Size > 0 ->
                    {_, OldSessId, Order1} = gb_trees:take_smallest(Order0),
                    OldestSession = lookup(Cb, Store0, OldSessId),
                    case ssl_session:valid_session(OldestSession, Lifetime) of
                        true ->
                            Store = update(Cb, Store0, SessionId, Session#session{time_stamp = TimeStamp}),
                            State0#state{db = Store,
                                         session_order = gb_trees:insert(InternalId, SessionId, Order0)};
                        false ->
                            %% Throw away oldest session as it is not valid anymore
                            Store1 = delete(Cb, Store0, OldSessId),
                            Store = update(Cb, Store1, SessionId, Session#session{time_stamp = TimeStamp}),
                            State0#state{db = Store,
                                         session_order =  gb_trees:insert(InternalId, SessionId, Order1)}
                    end;
                0 ->
                    Store = update(Cb, Store0, SessionId, Session#session{time_stamp = TimeStamp}),
                    State0#state{db = Store,
                                 session_order = gb_trees:insert(InternalId, SessionId, Order0)}
            end,
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()}.
handle_info({'DOWN', Monitor, _, _, _}, #state{listener = Monitor} = State) ->
     {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
call(Pid, Msg) ->
    try gen_server:call(Pid, Msg, infinity)
    catch
	exit:Reason ->
	    {no_server, Reason}
    end.

session_id(Key) ->
    Unique1 = erlang:unique_integer(),
    Unique2 = erlang:unique_integer(),
    %% Obfuscate to avoid DoS attack possibilities
    %% This id should be unpredictable an 32 bytes
    %% and unique but have no other cryptographic requirements.
    Bin1 = crypto:crypto_one_time(aes_128_ecb, Key, <<Unique1:128>>, true),
    Bin2 = crypto:crypto_one_time(aes_128_ecb, Key, <<Unique2:128>>, true),
    <<Bin1/binary, Bin2/binary>>.

invalidate_session(Cb, Store0, Order, SessionId, InternalId) ->
    Store = delete(Cb, Store0, SessionId),
    {Store, gb_trees:delete(InternalId, Order)}.

init(Cb, Options) ->
    Cb:init(Options).

lookup(Cb, Cache, Key) ->
    Cb:lookup(Cache, Key).

update(ssl_server_session_cache_db = Cb, Cache, Key, Session) ->
    Cb:update(Cache, Key, Session);
update(Cb, Cache, Key, Session) ->
    Cb:update(Cache, Key, Session),
    Cache.

delete(ssl_server_session_cache_db = Cb, Cache, Key) ->
    Cb:delete(Cache, Key);
delete(Cb, Cache, Key) ->
    Cb:delete(Cache, Key),
    Cache.

size(Cb,Cache) ->
    try Cb:size(Cache) of
        Size ->
            Size
    catch
        error:undef ->
            Cb:foldl(fun(_, Acc) -> Acc + 1 end, 0, Cache)
    end.

monitor_listener(ssl_unknown_listener) ->
    %% Backwards compatible Erlang node
    %% global process.
    undefined;
monitor_listener(Listen) ->
    inet:monitor(Listen).
