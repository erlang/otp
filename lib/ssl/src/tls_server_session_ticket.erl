%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
%% Purpose: Handle server side TLS-1.3 session ticket storage 
%%----------------------------------------------------------------------

-module(tls_server_session_ticket).
-moduledoc false.
-behaviour(gen_server).

-include("tls_handshake_1_3.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_cipher.hrl").

%% API
-export([start_link/7,
         new/4,
         use/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%% Tracing
-export([handle_trace/3]).

-define(SERVER, ?MODULE).

-record(state, {
                stateless,
                stateful,
                nonce,
                lifetime,
                max_early_data_size,
                listen_monitor
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(term(), Mode, integer(), integer(), integer(), tuple(), Seed) ->
                      {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore
    when Mode :: stateful | stateless |  stateful_with_cert | stateless_with_cert,
         Seed :: undefined | binary().
start_link(Listener, Mode1, Lifetime, TicketStoreSize, MaxEarlyDataSize, AntiReplay, Seed) ->
    Mode = case Mode1 of
               stateful_with_cert -> stateful;
               stateless_with_cert -> stateless;
               _ -> Mode1
    end,
    gen_server:start_link(?MODULE, [Listener, Mode, Lifetime, TicketStoreSize,
                                    MaxEarlyDataSize, AntiReplay, Seed], []).

new(Pid, Prf, MasterSecret, PeerCert) ->
    gen_server:call(Pid, {new_session_ticket, Prf, MasterSecret, PeerCert}, infinity).

use(Pid, Identifiers, Prf, HandshakeHist) ->
    gen_server:call(Pid, {use_ticket, Identifiers, Prf, HandshakeHist}, 
                    infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()}.                             
init([Listener | Args]) ->
    process_flag(trap_exit, true),
    proc_lib:set_label({tls_13_server_session_tickets, Listener}),
    Monitor = inet:monitor(Listener),
    State = initial_state(Args),
    {ok, State#state{listen_monitor = Monitor}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} .
handle_call({new_session_ticket, Prf, MasterSecret, PeerCert}, _From,
            #state{nonce = Nonce, 
                   lifetime = LifeTime,
                   max_early_data_size = MaxEarlyDataSize,
                   stateful = #{id_generator := IdGen}} = State0) -> 
    Id = stateful_psk_ticket_id(IdGen),
    PSK = tls_v1:pre_shared_key(MasterSecret, ticket_nonce(Nonce), Prf),
    SessionTicket = new_session_ticket(Id, Nonce, LifeTime, MaxEarlyDataSize),
    State = stateful_ticket_store(Id, SessionTicket, Prf, PSK, PeerCert, State0),
    {reply, SessionTicket, State};
handle_call({new_session_ticket, Prf, MasterSecret, PeerCert}, _From,
            #state{nonce = Nonce, 
                   stateless = #{}} = State) -> 
    BaseSessionTicket = new_session_ticket_base(State),
    SessionTicket = generate_stateless_ticket(BaseSessionTicket, Prf, 
                                              MasterSecret, PeerCert, State),
    {reply, SessionTicket, State#state{nonce = Nonce+1}};
handle_call({use_ticket, Identifiers, Prf, HandshakeHist}, _From, 
            #state{stateful = #{}} = State0) -> 
    {Result, State} = stateful_use(Identifiers, Prf, 
                                        HandshakeHist, State0),
    {reply, Result, State};
handle_call({use_ticket, Identifiers, Prf, HandshakeHist}, _From, 
            #state{stateless = #{}} = State0) -> 
    {Result, State} = stateless_use(Identifiers, Prf, 
                                         HandshakeHist, State0),
    {reply, Result, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
           {noreply, NewState :: term()}. 
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()}.
handle_info(rotate_bloom_filters, 
            #state{stateless = #{bloom_filter := BloomFilter0,
                                 warm_up_windows_remaining := WarmUp0,
                                 window := Window} = Stateless} = State) ->
    BloomFilter = tls_bloom_filter:rotate(BloomFilter0),
    erlang:send_after(Window * 1000, self(), rotate_bloom_filters),
    WarmUp = max(WarmUp0 - 1, 0),
    {noreply, State#state{stateless = Stateless#{bloom_filter => BloomFilter,
                                                 warm_up_windows_remaining => WarmUp}}};
handle_info({'DOWN', Monitor, _, _, _}, #state{listen_monitor = Monitor} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
initial_state([stateless, Lifetime, _, MaxEarlyDataSize, undefined, Seed]) ->
    #state{nonce = 0,
           stateless = #{seed => stateless_seed(Seed),
                         window => undefined},
           lifetime = Lifetime,
           max_early_data_size = MaxEarlyDataSize
          };
initial_state([stateless, Lifetime, _, MaxEarlyDataSize, {Window, K, M}, Seed]) ->
    erlang:send_after(Window * 1000, self(), rotate_bloom_filters),
    #state{nonce = 0,
           stateless = #{bloom_filter => tls_bloom_filter:new(K, M),
                         warm_up_windows_remaining => warm_up_windows(Seed),
                         seed => stateless_seed(Seed),
                         window => Window},
           lifetime = Lifetime,
           max_early_data_size = MaxEarlyDataSize
          };
initial_state([stateful, Lifetime, TicketStoreSize, MaxEarlyDataSize|_]) ->
    %% statfeful servers replay
    %% protection is that it saves
    %% all valid tickets
    #state{lifetime = Lifetime,
           max_early_data_size = MaxEarlyDataSize,
           nonce = 0,
           stateful = #{db => stateful_store(),                    
                        max => TicketStoreSize,
                        ref_index => #{},
                        id_generator => crypto:strong_rand_bytes(16)
                       }
          }.

ticket_age_add() ->
    MaxTicketAge = 7 * 24 * 3600 * 1000,
    IntMax = round(math:pow(2,32)) - 1,
    MaxAgeAdd = IntMax - MaxTicketAge,
    <<?UINT32(I)>> = crypto:strong_rand_bytes(4),
    case I > MaxAgeAdd of
        true ->
            I - MaxTicketAge;
        false ->
            I
    end.

ticket_nonce(I) ->
    <<?UINT64(I)>>.

new_session_ticket_base(#state{nonce = Nonce,
                               lifetime = Lifetime,
                               max_early_data_size = MaxEarlyDataSize}) ->
    new_session_ticket(undefined, Nonce, Lifetime, MaxEarlyDataSize).

new_session_ticket(Id, Nonce, Lifetime, MaxEarlyDataSize) ->
    TicketAgeAdd = ticket_age_add(),
    Extensions = #{early_data =>
                       #early_data_indication_nst{
                          indication = MaxEarlyDataSize}},
    #new_session_ticket{
       ticket = Id,
       ticket_lifetime = Lifetime,
       ticket_age_add = TicketAgeAdd,
       ticket_nonce = ticket_nonce(Nonce),
       extensions = Extensions
      }.


validate_binder(Binder, HandshakeHist, PSK, Prf, AlertDetail) ->
      case tls_handshake_1_3:is_valid_binder(Binder, HandshakeHist, PSK, Prf) of
          true ->
              true;
          false ->
              {error, ?ALERT_REC(?FATAL, ?ILLEGAL_PARAMETER, AlertDetail)}
      end.

%%%===================================================================
%%% Stateful store 
%%%===================================================================

stateful_store() ->
    gb_trees:empty().

stateful_ticket_store(Ref, NewSessionTicket, Hash, Psk, PeerCert,
                      #state{nonce = Nonce, 
                             stateful = #{db := Tree0, 
                                          max := Max,
                                          ref_index := Index0} = Stateful} 
                      = State0) ->    
    Id = {erlang:monotonic_time(), erlang:unique_integer([monotonic])},
    StatefulTicket = {NewSessionTicket, Hash, Psk, PeerCert},
    case gb_trees:size(Tree0) of
        Max ->
            %% Trow away oldest ticket
            {_, {#new_session_ticket{ticket = OldRef},_,_,_}, Tree1}
                = gb_trees:take_smallest(Tree0),
            Tree = gb_trees:insert(Id, StatefulTicket, Tree1),
            Index = maps:without([OldRef], Index0),
            State0#state{nonce = Nonce+1, stateful = 
                             Stateful#{db => Tree, 
                                       ref_index => Index#{Ref => Id}}};
        _ ->
            Tree = gb_trees:insert(Id, StatefulTicket, Tree0),
            State0#state{nonce = Nonce+1, stateful = 
                             Stateful#{db => Tree, 
                                       ref_index => Index0#{Ref => Id}}}   
    end.

stateful_use(#offered_psks{
                identities = Identities,
                binders = Binders
               }, Prf, HandshakeHist, State) ->    
    stateful_use(Identities, Binders, Prf, HandshakeHist, 0, State).

stateful_use([], [], _, _, _, State) ->
    {{ok, undefined}, State};
stateful_use([#psk_identity{identity = Ref} | Refs], [Binder | Binders], 
             Prf, HandshakeHist, Index, 
             #state{stateful = #{db := Tree0, 
                                 ref_index := RefIndex0} = Stateful} = State) ->
    try maps:get(Ref, RefIndex0) of
        Key ->
            case stateful_usable_ticket(Key, Prf, Binder, 
                                        HandshakeHist, Tree0) of
                true ->
                    RefIndex = maps:without([Ref], RefIndex0),
                    {{_,_, PSK, PeerCert}, Tree} = gb_trees:take(Key, Tree0),
                    {{ok, {Index, PSK, PeerCert}},
                     State#state{stateful = Stateful#{db => Tree, 
                                                      ref_index => RefIndex}}};
                false ->
                    stateful_use(Refs, Binders, Prf, 
                                 HandshakeHist, Index + 1, State);
                {error, _} = Error ->
                    {Error, State}
            end
    catch
        _:{badkey, Ref} -> 
            stateful_use(Refs, Binders, Prf, HandshakeHist, Index + 1, State)
    end.

stateful_usable_ticket(Key, Prf, Binder, HandshakeHist, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        none ->
            false;
        {value, {NewSessionTicket, Prf, PSK, _PeerCert}} ->
            case stateful_living_ticket(Key, NewSessionTicket) of
                true ->
                    validate_binder(Binder, HandshakeHist, PSK, Prf, stateful);
                _ ->
                    false 
            end;
        _ ->
            false
    end.

stateful_living_ticket({TimeStamp,_}, 
                       #new_session_ticket{ticket_lifetime = LifeTime}) ->
    Now = erlang:monotonic_time(),
    Lived = erlang:convert_time_unit(Now-TimeStamp, native, seconds),
    Lived < LifeTime.


stateful_psk_ticket_id(Key) ->
    Unique = erlang:unique_integer(),
    %% Obfuscate to avoid DoS attack possibilities
    %% that could invalidate tickets and render them
    %% unusable. This id should be unpredictable
    %% and unique but have no other cryptographic requirements.
    crypto:crypto_one_time(aes_128_ecb, Key, <<Unique:128>>, true).

%%%===================================================================
%%% Stateless ticket 
%%%===================================================================
generate_stateless_ticket(#new_session_ticket{ticket_nonce = Nonce, 
                                              ticket_age_add = TicketAgeAdd,
                                              ticket_lifetime = Lifetime} 
                         = Ticket, Prf, MasterSecret, PeerCert,
                         #state{stateless = #{seed := {IV, Shard}}}) ->
    PSK = tls_v1:pre_shared_key(MasterSecret, Nonce, Prf),
    Timestamp = erlang:system_time(second),
    Encrypted = ssl_cipher:encrypt_ticket(#stateless_ticket{
                                             hash = Prf,
                                             pre_shared_key = PSK,
                                             ticket_age_add = TicketAgeAdd,
                                             lifetime = Lifetime,
                                             timestamp = Timestamp,
                                             certificate = PeerCert
                                            }, Shard, IV),
    Ticket#new_session_ticket{ticket = Encrypted}.

stateless_use(#offered_psks{
                 identities = Identities,
                 binders = Binders
                }, Prf, HandshakeHist, State) ->    
    stateless_use(Identities, Binders, Prf, HandshakeHist, 0, State).

stateless_use([], [], _, _, _, State) ->
    {{ok, undefined}, State};
stateless_use([#psk_identity{identity = Encrypted,
                             obfuscated_ticket_age = ObfAge} |  Ids], 
              [Binder | Binders], Prf, HandshakeHist, Index, 
              #state{stateless = #{seed := {IV, Shard},
                                   window := Window}} = State) ->
    case ssl_cipher:decrypt_ticket(Encrypted, Shard, IV) of
        #stateless_ticket{hash = Prf,
                          pre_shared_key = PSK,
                          certificate = PeerCert} = Ticket ->
            case stateless_usable_ticket(Ticket, ObfAge, Binder,
                                        HandshakeHist, Window) of
                true ->
                    stateless_anti_replay(Index, PSK, Binder, PeerCert, State);
                false ->
                    stateless_use(Ids, Binders, Prf, HandshakeHist, 
                                  Index+1, State);
                {error, _} = Error ->
                    {Error, State}
            end;
        _  ->
            stateless_use(Ids, Binders, Prf, HandshakeHist, Index+1, State)
    end.

stateless_usable_ticket(#stateless_ticket{hash = Prf,
                                          ticket_age_add = TicketAgeAdd,
                                          lifetime = Lifetime,
                                          timestamp = Timestamp,
                                          pre_shared_key = PSK}, ObfAge, 
                       Binder, HandshakeHist, Window) ->
    case stateless_living_ticket(ObfAge, TicketAgeAdd, Lifetime, 
                                 Timestamp, Window) of
        true ->
           validate_binder(Binder, HandshakeHist, PSK, Prf, stateless); 
        false ->
            false
    end.

stateless_living_ticket(0, _, _, _, _) ->
    true;
%% If `anti_replay` is not enabled, then a ticket is considered to be living
%% if it has not exceeded its lifetime.
%%
%% If `anti_replay` is enabled, we must additionally perform a freshness check
%% as is outlined in section 8.3 Freshness Checks - RFC 8446
stateless_living_ticket(ObfAge, TicketAgeAdd, Lifetime, Timestamp, Window) ->
    %% RealAge is the server's view of the age of the ticket in seconds.
    RealAge = erlang:system_time(second) - Timestamp,

    %% ReportedAge is the client's view of the age of the ticket in milliseconds.
    ReportedAge = ObfAge - TicketAgeAdd,

    %% DeltaAge is the difference of the client's view of the age of the ticket
    %% and the server's view of the age of the ticket in seconds.
    DeltaAge = abs(RealAge - (ReportedAge / 1000)),

    %% We ensure that both the client's view of the age of the ticket and the
    %% server's view of the age of the ticket do not exceed the lifetime specified.
    (ReportedAge =< Lifetime * 1000)
        andalso (RealAge =< Lifetime)
        andalso (in_window(DeltaAge, Window)).

in_window(_, undefined) ->
    true;
%% RFC 8446 - section 8.2 Client Hello Recording
%% describes an anti-replay implementation that can use bounded memory
%% by storing a unique value from a ClientHello (in our case the PSK binder)
%% withing a given time window.
%%
%% In order implement this, when a ClientHello is received, the server
%% must ensure that a ClientHello has been sent relatively recently.
%% We do this by ensuring that the client and server view of the age
%% of the ticket is not larger than our recording window.
%%
%% In the case of an attempted replay attack, there are 2 possible
%% outcomes:
%%   - A ClientHello is replayed within the recording window
%%      * The ticket looks valid, `in_window` returns true
%%        so we proceed to check the unique value
%%      * The unique value (PSK Binder) is stored in the bloom filter
%%        and we reject the ticket.
%%
%%   - A ClientHello is replayed outside the recording window
%%      * We reject the ticket as `in_window` returns false.
in_window(Age, Window) when is_integer(Window) ->
    Age =< Window.

stateless_anti_replay(_Index, _PSK, _Binder, _PeerCert,
                      #state{stateless = #{warm_up_windows_remaining := WarmUpRemaining}
                            } = State) when WarmUpRemaining > 0 ->
    %% Reject all tickets during the warm-up period:
    %% RFC 8446 8.2 Client Hello Recording
    %% "When implementations are freshly started, they SHOULD reject 0-RTT as
    %% long as any portion of their recording window overlaps the startup time."
    {{ok, undefined}, State};
stateless_anti_replay(Index, PSK, Binder, PeerCert,
                      #state{stateless = #{bloom_filter := BloomFilter0} 
                             = Stateless} = State) ->
    case tls_bloom_filter:contains(BloomFilter0, Binder) of
        true ->
            %%possible_replay
            {{ok, undefined}, State};
        false ->
            BloomFilter = tls_bloom_filter:add_elem(BloomFilter0, Binder),
            {{ok, {Index, PSK, PeerCert}},
             State#state{stateless = Stateless#{bloom_filter => BloomFilter}}}
    end;
stateless_anti_replay(Index, PSK, _Binder, PeerCert, State) ->
     {{ok, {Index, PSK, PeerCert}}, State}.

-spec stateless_seed(Seed :: undefined | binary()) ->
          {IV :: binary(), Shard :: binary()}.
stateless_seed(undefined) ->
    {crypto:strong_rand_bytes(16), crypto:strong_rand_bytes(32)};
stateless_seed(Seed) ->
    <<IV:16/binary, Shard:32/binary, _/binary>> = crypto:hash(sha512, Seed),
    {IV, Shard}.

-spec warm_up_windows(Seed :: undefined | binary()) -> 0 | 2.
warm_up_windows(undefined) ->
    0;
warm_up_windows(_) ->
    %% When the encryption seed is specified, "warm up" the bloom filter for
    %% 2*WindowSize to ensure tickets from a previous instance of the server
    %% (before a restart) cannot be reused, if the ticket encryption seed is reused.
    2.

%%%################################################################
%%%#
%%%# Tracing
%%%#
handle_trace(rle, {call, {?MODULE, init, [[ListenSocket, Mode, Lifetime,
                                           StoreSize | _T]]}}, Stack) ->
    {io_lib:format("(*server) ([ListenSocket = ~w Mode = ~w Lifetime = ~w "
                   "StoreSize = ~w, ...])",
                   [ListenSocket, Mode, Lifetime, StoreSize]),
     [{role, server} | Stack]};
handle_trace(ssn,
             {call, {?MODULE, terminate, [Reason, _State]}}, Stack) ->
    {io_lib:format("(Reason ~w)", [Reason]), Stack};
handle_trace(ssn,
             {call, {?MODULE, handle_call,
                     [CallTuple, _From, _State]}}, Stack) ->
    {io_lib:format("(Call = ~w)", [element(1, CallTuple)]), Stack};
handle_trace(ssn,
             {call, {?MODULE,handle_call,
                     [{Call = use_ticket,
                       {offered_psks,
                        [{psk_identity, PskIdentity, _ObfAge}],
                        [Binder]},
                       _Prf,
                       _HandshakeHist}, _From, _State]}}, Stack) ->
    {io_lib:format("(Call = ~w PskIdentity = ~W Binder = ~W)",
                  [Call, PskIdentity, 5, Binder, 5]), Stack};
handle_trace(ssn,
             {call, {?MODULE, validate_binder,
                     [Binder, _HandshakeHist, PSK, _Prf, _AlertDetail]}}, Stack) ->
    {io_lib:format("(Binder = ~W PSK = ~W)", [Binder, 5, PSK, 5]), Stack};
handle_trace(ssn,
             {call, {?MODULE, initial_state,
                     [[Mode, _Lifetime, _StoreSize,_MaxEarlyDataSize,
                       Window, Seed]]}}, Stack) ->
    {io_lib:format("(Mode = ~w Window = ~w Seed = ~W)", [Mode, Window, Seed, 5]), Stack};
handle_trace(ssn,
             {call, {?MODULE, generate_stateless_ticket,
                     [_BaseTicket, _Prf, MasterSecret, _State]}}, Stack) ->
    {io_lib:format("(MasterSecret = ~W)", [MasterSecret, 5]), Stack};
handle_trace(ssn,
             {call, {?MODULE, stateless_use,
                     [[{psk_identity, Encrypted, _ObfAge} | _],
                      [Binder | _],
                      _Prf, _HandshakeHist, _Index, _State]}}, Stack) ->
    {io_lib:format("(Encrypted = ~W Binder = ~W)", [Encrypted, 5, Binder, 5]), Stack};
handle_trace(ssn,
             {call, {?MODULE, in_window, [RealAge, Window]}}, Stack) ->
    {io_lib:format("(RealAge = ~w Window = ~w)",
                   [RealAge, Window]), Stack};
handle_trace(ssn,
             {call, {?MODULE, stateless_usable_ticket,
                     [{stateless_ticket, _Prf, _PreSharedKey, _TicketAgeAdd,
                       _Lifetime, _Timestamp}, _ObfAge, Binder,
                      _HandshakeHist, Window]}}, Stack) ->
    {io_lib:format("(Binder = ~W Window = ~w)", [Binder, 5, Window]), Stack};
handle_trace(ssn,
             {call, {?MODULE, stateless_anti_replay,
                     [_Index, PSK, _Binder, _State]}}, Stack) ->
    {io_lib:format("(PSK = ~W)", [PSK, 5]), Stack};
handle_trace(ssn,
             {return_from, {?MODULE, stateless_use, 6},
              {{ok, {_Index, PSK}}, _State}}, Stack) ->
    {io_lib:format("PSK = ~W", [PSK, 5]), Stack};
handle_trace(ssn,
             {return_from, {?MODULE, generate_stateless_ticket, 4},
              {new_session_ticket, _LifeTime, _AgeAdd, _Nonce, Ticket,
               _Extensions}}, Stack) ->
    {io_lib:format("Ticket = ~W", [Ticket, 5]), Stack};
handle_trace(ssn,
             {return_from, {?MODULE, initial_state, 1},
              {state, _Stateless = #{seed := {IV, Shard}, window := Window},
               _Stateful = undefined, _Nonce, _Lifetime ,
               _MaxEarlyDataSize, ListenMonitor}}, Stack) ->
    {io_lib:format("IV = ~W Shard = ~W Window = ~w ListenMonitor = ~w",
                   [IV, 5, Shard, 5, Window, ListenMonitor]), Stack};
handle_trace(ssn,
             {return_from, {?MODULE, stateless_anti_replay, 4},
              {{ok, {_Index, PSK}}, _State}}, Stack) ->
    {io_lib:format("ticket OK ~W", [PSK, 5]), Stack};
handle_trace(ssn,
             {return_from, {?MODULE, stateless_anti_replay, 4},
              Return}, Stack) ->
    {io_lib:format("ticket REJECTED ~W", [Return, 5]), Stack}.
