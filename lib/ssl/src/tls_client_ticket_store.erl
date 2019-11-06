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
%% Purpose: Handle client side TLS-1.3 session ticket storage
%%----------------------------------------------------------------------

-module(tls_client_ticket_store).
-behaviour(gen_server).

-include("tls_handshake_1_3.hrl").

%% API
-export([find_ticket/1,
         get_tickets/1,
         remove_ticket/1,
         start_link/2,
         store_ticket/4,
         update_ticket/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                db,
                lifetime,
                max
               }).

-record(data, {
               pos = undefined,
               hkdf,
               sni,
               psk,
               timestamp,
               ticket
              }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(integer(), integer()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Max, Lifetime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Max, Lifetime], []).

find_ticket(HashAlgos) ->
    %% TODO use also SNI when selecting tickets
    gen_server:call(?MODULE, {find_ticket, HashAlgos}, infinity).

get_tickets(Keys) ->
    gen_server:call(?MODULE, {get_tickets, Keys}, infinity).

remove_ticket(Key) ->
    gen_server:call(?MODULE, {remove_ticket, Key}, infinity).

store_ticket(Ticket, HKDF, SNI, PSK) ->
    gen_server:call(?MODULE, {store_ticket, Ticket, HKDF, SNI, PSK}, infinity).

update_ticket(Key, Pos) ->
    gen_server:call(?MODULE, {update_ticket, Key, Pos}, infinity).

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
handle_call({find_ticket, HashAlgos}, _From, State) ->
    Key = find_ticket(State, HashAlgos),
    {reply, Key, State};
handle_call({get_tickets, Keys}, _From, State) ->
    Data = get_tickets(State, Keys),
    {reply, Data, State};
handle_call({remove_ticket, Key}, _From, State0) ->
    State = remove_ticket(State0, Key),
    {reply, ok, State};
handle_call({store_ticket, Ticket, HKDF, SNI, PSK}, _From, State0) ->
    State = store_ticket(State0, Ticket, HKDF, SNI, PSK),
    {reply, ok, State};
handle_call({update_ticket, Key, Pos}, _From, State0) ->
    State = update_ticket(State0, Key, Pos),
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()}.
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

inital_state([Max, Lifetime]) ->
    #state{db = gb_trees:empty(),
           lifetime = Lifetime,
           max = Max
          }.


find_ticket(_, []) ->
    undefined;
find_ticket(#state{db = Db,
                   lifetime = Lifetime} = State, [Hash|T]) ->
    case iterate_tickets(gb_trees:iterator(Db), Hash, Lifetime) of
        none ->
            find_ticket(State, T);
        Key ->
            Key
    end.

iterate_tickets(Iter0, Hash, Lifetime) ->
    case gb_trees:next(Iter0) of
        {Key, #data{hkdf = Hash,
                    timestamp = Timestamp}, Iter} ->
            Age = erlang:system_time(seconds) - Timestamp,
            if Age < Lifetime ->
                    Key;
               true ->
                    iterate_tickets(Iter, Hash, Lifetime)
            end;
        {_, _, Iter} ->
            iterate_tickets(Iter, Hash, Lifetime);
        none ->
            none
    end.


get_tickets(State, Keys) ->
    get_tickets(State, Keys, []).
%%
get_tickets(_, [], []) ->
    undefined; %% No tickets found
get_tickets(_, [], Acc) ->
    Acc;
get_tickets(#state{db = Db} = State, [Key|T], Acc) ->
    try gb_trees:get(Key, Db) of
        #data{pos = Pos,
              hkdf = HKDF,
              psk = PSK,
              timestamp = Timestamp,
              ticket = NewSessionTicket} ->
            #new_session_ticket{
               ticket_lifetime = _LifeTime,
               ticket_age_add = AgeAdd,
               ticket_nonce = Nonce,
               ticket = Ticket,
               extensions = _Extensions
              } = NewSessionTicket,
            TicketAge =  erlang:system_time(seconds) - Timestamp,
            ObfuscatedTicketAge = obfuscate_ticket_age(TicketAge, AgeAdd),
            Identity = #psk_identity{
                          identity = Ticket,
                          obfuscated_ticket_age = ObfuscatedTicketAge},
            get_tickets(State, T, [{Key, Pos, Identity, PSK, Nonce, HKDF}|Acc])
    catch
        _:_ ->
            get_tickets(State, T, Acc)
    end.

%% The "obfuscated_ticket_age"
%% field of each PskIdentity contains an obfuscated version of the
%% ticket age formed by taking the age in milliseconds and adding the
%% "ticket_age_add" value that was included with the ticket
%% (see Section 4.6.1), modulo 2^32.
obfuscate_ticket_age(TicketAge, AgeAdd) ->
    (TicketAge + AgeAdd) rem round(math:pow(2,32)).


remove_ticket(#state{db = Db0} = State, Key) ->
    Db = gb_trees:delete_any(Key, Db0),
    State#state{db = Db}.


store_ticket(#state{db = Db0, max = Max} = State, Ticket, HKDF, SNI, PSK) ->
    Timestamp = erlang:system_time(seconds),
    Size = gb_trees:size(Db0),
    Db1 = if Size =:= Max ->
                  delete_oldest(Db0);
             true ->
                  Db0
          end,
    Db = gb_trees:insert(erlang:monotonic_time(),
                         #data{hkdf = HKDF,
                               sni = SNI,
                               psk = PSK,
                               timestamp = Timestamp,
                               ticket = Ticket},
                         Db1),
    State#state{db = Db}.


update_ticket(#state{db = Db0} = State, Key, Pos) ->
    try gb_trees:get(Key, Db0) of
        Value ->
            Db = gb_trees:update(Key, Value#data{pos = Pos}, Db0),
            State#state{db = Db}
    catch
        _:_ ->
            State
    end.


delete_oldest(Db0) ->
    try gb_trees:take_smallest(Db0) of
        {_, _, Db} ->
            Db
    catch
        _:_ ->
            Db0
    end.
