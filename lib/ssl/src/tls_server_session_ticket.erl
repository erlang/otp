%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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
-behaviour(gen_server).

-include("tls_handshake_1_3.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("ssl_cipher.hrl").

%% API
-export([start_link/3,
         new/3,
         use/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                stateless,
                stateful,
                nonce,
                lifetime
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(atom(), integer(), tuple()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Mode, Lifetime, AntiReplay) ->
    gen_server:start_link(?MODULE, [Mode, Lifetime, AntiReplay], []).

new(Pid, Prf, MasterSecret) ->
    gen_server:call(Pid, {new_session_ticket, Prf, MasterSecret}, infinity).

use(Pid, Identifiers, Prf, HandshakeHist) ->
    gen_server:call(Pid, {use_ticket, Identifiers, Prf, HandshakeHist}, 
                    infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()}.                             
init(Args) ->
    process_flag(trap_exit, true),
    State = inital_state(Args),
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} .
handle_call({new_session_ticket, Prf, MasterSecret}, _From, 
            #state{nonce = Nonce, 
                   lifetime = LifeTime,
                   stateful = #{}} = State0) -> 
    Id = stateful_psk_id(),
    PSK = tls_v1:pre_shared_key(MasterSecret, ticket_nonce(Nonce), Prf),
    SessionTicket = new_session_ticket(Id, Nonce, LifeTime),
    State = stateful_ticket_store(Id, SessionTicket, Prf, PSK, State0),
    {reply, SessionTicket, State};
handle_call({new_session_ticket, Prf, MasterSecret}, _From, 
            #state{nonce = Nonce, 
                   stateless = #{}} = State) -> 
    BaseSessionTicket = new_session_ticket_base(State),
    SessionTicket = generate_statless_ticket(BaseSessionTicket, Prf, 
                                             MasterSecret, State),
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
                                 window := Window} = Stateless} = State) ->
    BloomFilter = tls_bloom_filter:rotate(BloomFilter0),
    erlang:send_after(Window * 1000, self(), rotate_bloom_filters),
    {noreply, State#state{stateless = Stateless#{bloom_filter => BloomFilter}}};
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

inital_state([stateless, Lifetime, undefined]) ->
    #state{nonce = 0,
           stateless = #{seed => {crypto:strong_rand_bytes(16), 
                                  crypto:strong_rand_bytes(32)},
                         window => undefined},
           lifetime = Lifetime
          };
inital_state([stateless, Lifetime, {Window, K, M}]) ->
    erlang:send_after(Window * 1000, self(), rotate_bloom_filters),
    #state{nonce = 0,
           stateless = #{bloom_filter => tls_bloom_filter:new(K, M),
                         seed => {crypto:strong_rand_bytes(16),
                                  crypto:strong_rand_bytes(32)},
                         window => Window},
           lifetime = Lifetime
          };
inital_state([stateful, Lifetime|_]) ->
    %% statfeful servers replay
    %% protection is that it saves
    %% all valid tickets
    #state{lifetime = Lifetime,
           nonce = 0,
           stateful = #{db => stateful_store(),                    
                        max => 1000,
                        ref_index => #{}
                       }
          }.

ticket_age_add() ->
    MaxTicketAge = 7 * 24 * 3600,
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
                               lifetime = Lifetime}) ->
    new_session_ticket(undefined, Nonce, Lifetime).

new_session_ticket(Id, Nonce, Lifetime) ->
    TicketAgeAdd = ticket_age_add(),
    #new_session_ticket{
       ticket = Id,
       ticket_lifetime = Lifetime,
       ticket_age_add = TicketAgeAdd,
       ticket_nonce = ticket_nonce(Nonce),
       extensions = #{}
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

stateful_ticket_store(Ref, NewSessionTicket, Hash, Psk, 
                      #state{nonce = Nonce, 
                             stateful = #{db := Tree0, 
                                          max := Max,
                                          ref_index := Index0} = Stateful} 
                      = State0) ->    
    Id = erlang:monotonic_time(),
    StatefulTicket = {NewSessionTicket, Hash, Psk},
    case gb_trees:size(Tree0) of
        Max ->
            %% Trow away oldes ticket
            {_, {#new_session_ticket{ticket = OldRef},_,_}, Tree1} 
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
                    {{_,_, PSK}, Tree} = gb_trees:take(Key, Tree0),
                    {{ok, {Index, PSK}}, 
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
        {value, {NewSessionTicket, Prf, PSK}} ->
            case stateful_living_ticket(Key, NewSessionTicket) of
                true ->
                    validate_binder(Binder, HandshakeHist, PSK, Prf, stateful);
                _ ->
                    false 
            end;
        _ ->
            false
    end.

stateful_living_ticket(TimeStamp, 
                       #new_session_ticket{ticket_lifetime = LifeTime}) ->
    Now = erlang:monotonic_time(),
    Lived = erlang:convert_time_unit(Now-TimeStamp, native, seconds),
    Lived < LifeTime.


stateful_psk_id() ->
    term_to_binary(make_ref()).

%%%===================================================================
%%% Stateless ticket 
%%%===================================================================
generate_statless_ticket(#new_session_ticket{ticket_nonce = Nonce, 
                                             ticket_age_add = TicketAgeAdd,
                                             ticket_lifetime = Lifetime} 
                         = Ticket, Prf, MasterSecret, 
                         #state{stateless = #{seed := {IV, Shard}}}) ->
    PSK = tls_v1:pre_shared_key(MasterSecret, Nonce, Prf),
    Timestamp = erlang:system_time(second),
    Encrypted = ssl_cipher:encrypt_ticket(#stateless_ticket{
                                             hash = Prf,
                                             pre_shared_key = PSK,
                                             ticket_age_add = TicketAgeAdd,
                                             lifetime = Lifetime,
                                             timestamp = Timestamp
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
                          pre_shared_key = PSK} = Ticket ->
            case statless_usable_ticket(Ticket, ObfAge, Binder,
                                        HandshakeHist, Window) of
                true ->
                    stateless_anti_replay(Index, PSK, Binder, State);
                false ->
                    stateless_use(Ids, Binders, Prf, HandshakeHist, 
                                  Index+1, State);
                {error, _} = Error ->
                    {Error, State}
            end;
        _  ->
            stateless_use(Ids, Binders, Prf, HandshakeHist, Index+1, State)
    end.

statless_usable_ticket(#stateless_ticket{hash = Prf,
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
stateless_living_ticket(ObfAge, TicketAgeAdd, Lifetime, Timestamp, Window) ->
    ReportedAge = ObfAge - TicketAgeAdd,
    RealAge = erlang:system_time(second) - Timestamp,
    (ReportedAge =< Lifetime)
        andalso (RealAge =< Lifetime)
        andalso (in_window(RealAge, Window)).
        
in_window(_, undefined) ->
    true;
in_window(Age, {Window, _, _}) ->
    Age =< Window.

stateless_anti_replay(Index, PSK, Binder, 
                      #state{stateless = #{bloom_filter := BloomFilter0} 
                             = Stateless} = State) ->
    case tls_bloom_filter:contains(BloomFilter0, Binder) of
        true ->
            %%possible_replay
            {{ok, undefined}, State};
        false ->
            BloomFilter = tls_bloom_filter:add_elem(BloomFilter0, Binder),
            {{ok, {Index, PSK}},
             State#state{stateless = Stateless#{bloom_filter => BloomFilter}}}
    end;
stateless_anti_replay(Index, PSK, _, State) ->
     {{ok, {Index, PSK}}, State}.
